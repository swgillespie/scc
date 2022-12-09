#include "scc.h"

/**
 * SCC is not (yet) a full C compiler in that it does not provide a
 * preprocessor.
 *
 * Until it does, we rely on another C compiler to preprocess C source that we
 * can feed as an input to SCC. We do this by forking off a child process that
 * invokes a C compiler and feeds its stdout to our tokenizer.
 */
static FILE*
setup_preprocessor(char* filename)
{
  int pipefd[2];
  if (pipe(pipefd) != 0) {
    perror("failed to open pipe");
    exit(1);
  }

  // We open the source file in the parent process to issue an error earlier if
  // the file doesn't exist.
  FILE* f = fopen(filename, "r");
  if (!f) {
    perror("failed to open source file");
    exit(1);
  }

  int child_pid = fork();
  if (child_pid == -1) {
    perror("failed to fork");
    exit(1);
  }

  if (child_pid == 0) {
    // The child side of the fork. The child writes preprocessed C source to the
    // pipe.
    close(pipefd[0]);
    dup2(pipefd[1], 1 /* stdout */);
    close(pipefd[1]);
    char* args[] = { "/usr/bin/clang", "-E", "-P", filename, NULL };
    execv("/usr/bin/clang", args); // does not return
    exit(0);
  } else {
    // The parent side of the fork. The parent sets the tokenizer's FILE to be
    // the read end of the pipe coming from the C preprocessor.
    close(pipefd[1]);
    fclose(f);
    return fdopen(pipefd[0], "r");
  }
}

int
main(int argc, char** argv)
{
  if (argc != 2) {
    puts("usage: scc <input.c>");
    return 1;
  }

  FILE* stream = setup_preprocessor(argv[1]);
  load_file(argv[1], stream);
  token* tok = tokenize();
  symbol* sym = parse(&tok);
  codegen(sym);
}