#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>

int const dirBufSize = 1024;

char const *const usage = "Usage: wd DIR CMD [ARGS]\n";

int main(int argc, char *argv[])
{
  if (argc <= 2)
  {
    fprintf(stderr, "Error: At least 2 arguments are necessary.\n");
    fprintf(stderr, usage);
    return EXIT_FAILURE;
  }
  {
    char originalDir[dirBufSize];
    char *dir = argv[1];
    char *cmd = argv[2];
    char *newArgs[argc];
    {
      int newIndex;
      newArgs[0] = cmd;
      {
        int index;
        for (index = 3, newIndex = 1; index < argc; index++, newIndex++)
        {
          newArgs[newIndex] = argv[index];
        }
      }
      newArgs[newIndex] = NULL;
    }
    if (NULL == getcwd(originalDir, dirBufSize))
    {
      fprintf(stderr, "Error (%d): %s (%d)\n", __LINE__, strerror(errno), errno);
      return EXIT_FAILURE;
    }
    if (-1 == chdir(dir))
    {
      fprintf(stderr, "Error (%d): %s (%d)\n", __LINE__, strerror(errno), errno);
      return EXIT_FAILURE;
    }
    if (-1 == execvp(cmd, newArgs))
    {
      fprintf(stderr, "Error (%d): %s (%d)\n", __LINE__, strerror(errno), errno);
      if (-1 == chdir(originalDir))
      {
        fprintf(stderr, "Error (%d): %s (%d)\n", __LINE__, strerror(errno), errno);
      }
      return EXIT_FAILURE;
    }
    if (-1 == chdir(originalDir))
    {
      fprintf(stderr, "Error (%d): %s (%d)\n", __LINE__, strerror(errno), errno);
      return EXIT_FAILURE;
    }
  }
  return EXIT_SUCCESS;
}
