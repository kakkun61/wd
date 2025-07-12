#include <direct.h>
#include <errno.h>
#include <process.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <tchar.h>
#include <windows.h>

#define NEW_ARG_SIZE 64
#define ERR_BUF_SIZE 128
#define NEW_COMMAND_LINE_SIZE 2048

char const *const version = "1.2.0";

char const *const usage = "Usage: wd DIR CMD [ARGS]\n";

#define LOG_ERROR(stream, message)                                 \
  do {                                                             \
    logError(stream, message, GetLastError(), __FILE__, __LINE__); \
  } while (0)

void logError(FILE *const stream, char message[], DWORD const errorNo, char file[], int const line) {
  TCHAR errorMessage[ERR_BUF_SIZE];
  if (0 == FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, NULL, errorNo, MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
                         errorMessage, ERR_BUF_SIZE, NULL))
    fprintf(stream, "Error (%s:%d): %s: (%d)\n", file, line, message, errorNo);
  else
    _ftprintf(stream, _T("Error (%s:%d): %s: %s (%d)\n"), file, line, message, errorMessage, errorNo);
}

int ArgvToCommandLine(_TCHAR const *const argv[], LPTSTR commandLine, unsigned int const commandLineSize);

int _tmain(int argc, _TCHAR *argv[]) {
  if (argc <= 2) {
    fprintf(stderr, "Error: At least 2 arguments are necessary.\n%swd %s\n", usage, version);
    return EXIT_FAILURE;
  }
  {
    _TCHAR *dir = argv[1];
    _TCHAR *newArgs[NEW_ARG_SIZE];
    if (NEW_ARG_SIZE - 1 < argc - 2) {
      //    argv: wd dir cmd args...
      // newArgs:        cmd args... NULL
      fprintf(stderr, "Error: Arguments are too many. Its number must be less than %d.\n", NEW_ARG_SIZE - 1);
      return EXIT_FAILURE;
    }
    {
      int newIndex;
      {
        int index;
        for (index = 2, newIndex = 0; index < argc; index++, newIndex++) newArgs[newIndex] = argv[index];
      }
      newArgs[newIndex] = NULL;
    }
    {
      _TCHAR newCommandLine[NEW_COMMAND_LINE_SIZE] = { 0 };
      int const newCommandLineLength = ArgvToCommandLine(newArgs, newCommandLine, NEW_COMMAND_LINE_SIZE);
      switch (newCommandLineLength) {
        case -1:
          fprintf(stderr, "Error (%s:%d): ArgvToCommandLine: argv or commandLine is NULL\n", __FILE__, __LINE__);
          return EXIT_FAILURE;
        case -2:
          fprintf(stderr, "Error (%s:%d): ArgvToCommandLine: commandLineSize is too small\n", __FILE__, __LINE__);
          return EXIT_FAILURE;
        case -3:
          fprintf(stderr, "Error (%s:%d): ArgvToCommandLine: another unexpected error occurred\n", __FILE__, __LINE__);
          return EXIT_FAILURE;
      }
      {
        STARTUPINFO startupInfo = { 0 };
        PROCESS_INFORMATION processInformation = { 0 };
        startupInfo.cb = sizeof startupInfo;
        if (FALSE ==
            CreateProcess(NULL, newCommandLine, NULL, NULL, FALSE, 0, NULL, dir, &startupInfo, &processInformation)) {
          LOG_ERROR(stderr, "CreateProcess");
          return EXIT_FAILURE;
        }
        {
          DWORD err = WaitForSingleObject(processInformation.hProcess, INFINITE);
          switch (err) {
            case WAIT_ABANDONED:
              fprintf(stderr, "Error (%s:%d): WaitForSingleObject: WAIT_ABANDONED\n", __FILE__, __LINE__);
              return EXIT_FAILURE;
            case WAIT_FAILED:
              fprintf(stderr, "Error (%s:%d): WaitForSingleObject: WAIT_FAILED\n", __FILE__, __LINE__);
              return EXIT_FAILURE;
          }
        }
        CloseHandle(processInformation.hProcess);
        CloseHandle(processInformation.hThread);
      }
    }
  }
  return EXIT_SUCCESS;
}

#define ARGV_TO_COMMAND_LINE_BUFFER_SIZE 1024

/// \brief Converts an array of command line arguments into a single command line string.
/// \param argv
/// \param commandLine
/// \param commandLineSize
/// \return length of the resulting command line string
/// \retval -1 When argv or commandLine is NULL.
/// \retval -2 When commandLineSize is too small to hold the result or when an internal buffer is too small.
/// \retval -3 When another unexpected error occurs.
///
/// \par Example:
/// \code
/// argv[0] | argv[1] | argv[2] | result
/// a b c   | d       | e       | "a b c" d e
/// ab"c    | \       | d       | ab\"c \\ d
/// a\\\b   | de fg   | h       | a\\\b "de fg" h
/// a\"b    | c       | d       | a\\\"b c d"
/// a\\b c  | d       | e       | "a\\b c" d e
/// ab" c d |         |         | "ab\" c d"
/// \endcode
///
/// \see https://learn.microsoft.com/en-us/cpp/c-language/parsing-c-command-line-arguments
int ArgvToCommandLine(_TCHAR const *const argv[], LPTSTR commandLine, unsigned int const commandLineSize) {
  int const unexpectedArgError = -1;
  int const bufferTooSmallError = -2;
  int const anotherError = -3;
  if (NULL == argv || NULL == commandLine) return unexpectedArgError;

  unsigned int length = 0;
  for (int wordIndex = 0; argv[wordIndex] != NULL; wordIndex++) {
    if (length == commandLineSize) return bufferTooSmallError;
    _TCHAR buffer[ARGV_TO_COMMAND_LINE_BUFFER_SIZE];
    int bufferIndex = 0;
    BOOL needsQuotes = FALSE;
    int backslashCount = 0;
    for (int charIndex = 0; argv[wordIndex][charIndex] != _T('\0'); charIndex++) {
      if (argv[wordIndex][charIndex] == ' ') needsQuotes = TRUE;
      switch (argv[wordIndex][charIndex]) {
        case '\"':
          if (ARGV_TO_COMMAND_LINE_BUFFER_SIZE < bufferIndex + 2 * backslashCount + 1) return bufferTooSmallError;
          for (int i = 0; i < 2 * backslashCount + 1; i++) buffer[bufferIndex++] = _T('\\');
          backslashCount = 0;
          if (ARGV_TO_COMMAND_LINE_BUFFER_SIZE < bufferIndex + 1) return bufferTooSmallError;
          buffer[bufferIndex++] = argv[wordIndex][charIndex];
          break;
        case '\\':
          backslashCount++;
          break;
        default:
          if (0 < backslashCount) {
            if (ARGV_TO_COMMAND_LINE_BUFFER_SIZE < bufferIndex + backslashCount) return bufferTooSmallError;
            for (int i = 0; i < backslashCount; i++) buffer[bufferIndex++] = _T('\\');
            backslashCount = 0;
          }
          if (ARGV_TO_COMMAND_LINE_BUFFER_SIZE < bufferIndex + 1) return bufferTooSmallError;
          buffer[bufferIndex++] = argv[wordIndex][charIndex];
          break;
      }
    }
    if (0 < backslashCount) {
      if (ARGV_TO_COMMAND_LINE_BUFFER_SIZE < bufferIndex + backslashCount) return bufferTooSmallError;
      for (int i = 0; i < backslashCount; i++) buffer[bufferIndex++] = _T('\\');
    }
    if (ARGV_TO_COMMAND_LINE_BUFFER_SIZE < bufferIndex + 1) return bufferTooSmallError;
    buffer[bufferIndex] = _T('\0');
    if (needsQuotes) {
      if (commandLineSize < length + 2) return bufferTooSmallError;
      commandLine[length++] = _T('\"');
      commandLine[length] = _T('\0');
    }
    {
      errno_t err = _tcscat_s(commandLine, commandLineSize, buffer);
      switch (err) {
        case EINVAL:
          fprintf(stderr, "Error (%s:%d): _tcscat_s: EINVAL\n", __FILE__, __LINE__);
          return anotherError;
        case ERANGE:
          fprintf(stderr, "Error (%s:%d): _tcscat_s: ERANGE\n", __FILE__, __LINE__);
          return bufferTooSmallError;
      }
      length += _tcslen(buffer);
    }
    if (needsQuotes) {
      if (commandLineSize < length + 2) return bufferTooSmallError;
      commandLine[length++] = _T('\"');
      commandLine[length] = _T('\0');
    }
    if (argv[wordIndex + 1] != NULL) {
      if (commandLineSize < length + 2) return bufferTooSmallError;
      commandLine[length++] = _T(' ');
      commandLine[length] = _T('\0');
    }
  }
  if (commandLineSize < length + 1) return bufferTooSmallError;
  commandLine[length] = _T('\0');

  return length;
}
