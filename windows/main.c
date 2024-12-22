#include <direct.h>
#include <errno.h>
#include <process.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define DIR_BUF_SIZE 1024
#define NEW_ARG_SIZE 64
#define ERR_BUF_SIZE 128

char const *const version = "1.1.0";

char const *const usage = "Usage: wd DIR CMD [ARGS]\n";

#define LOG_ERROR(stream, message) \
  (logError(stream, message, errno, __FILE__, __LINE__))

void logError(FILE *const stream, char message[], int const errorNo, char file[], int const line)
{
  char errorMessage[ERR_BUF_SIZE];
  if (0 != strerror_s(errorMessage, ERR_BUF_SIZE, errorNo))
    fprintf(stream, "Error (%s:%d): %s: (%d)\n", file, line, message, errorNo);
  else
    fprintf(stream, "Error (%s:%d): %s: %s (%d)\n", file, line, message, errorMessage, errorNo);
}

int main(int argc, char *argv[])
{
  if (argc <= 2)
  {
    fprintf(stderr, "Error: At least 2 arguments are necessary.\n%swd %s\n", usage, version);
    return EXIT_FAILURE;
  }
  {
    char originalDir[DIR_BUF_SIZE];
    char *dir = argv[1];
    char *cmd = argv[2];
    char *newArgs[NEW_ARG_SIZE];
    if (NEW_ARG_SIZE - 1 < argc - 2) {
      //    argv: wd dir cmd args...
      // newArgs:        cmd args... NULL
      fprintf(stderr, "Error: Arguments are too many. Its number must be less than %d.\n", NEW_ARG_SIZE - 1);
      return EXIT_FAILURE;
    }
    {
      int newIndex;
      newArgs[0] = cmd;
      {
        int index;
        for (index = 3, newIndex = 1; index < argc; index++, newIndex++)
          newArgs[newIndex] = argv[index];
      }
      newArgs[newIndex] = NULL;
    }
    if (NULL == _getcwd(originalDir, DIR_BUF_SIZE))
    {
      LOG_ERROR(stderr, "_getcwd");
      return EXIT_FAILURE;
    }
    if (-1 == _chdir(dir))
    {
      LOG_ERROR(stderr, "_chdir");
      return EXIT_FAILURE;
    }
    {
      intptr_t const spawnResult = _spawnvp(_P_WAIT, (char const *) cmd, (char const *const *) newArgs);
      if (spawnResult == -1)
      {
        LOG_ERROR(stderr, "_spawnvp");
        if (-1 == _chdir(originalDir))
          LOG_ERROR(stderr, "_chdir");
        return EXIT_FAILURE;
      }
      if (-1 == _chdir(originalDir))
      {
        LOG_ERROR(stderr, "_chdir");
        return EXIT_FAILURE;
      }
      return (int) spawnResult;
    }
  }
  return EXIT_SUCCESS;
}
