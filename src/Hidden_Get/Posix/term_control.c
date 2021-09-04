#include <stdio.h>
#include <termios.h>
#include <unistd.h>
#include <stdlib.h>

struct termios oldattr;

struct termios* disable_echo()
{
  struct termios newattr, *oldattr;

  oldattr = malloc (sizeof (*oldattr));

  tcgetattr (STDIN_FILENO, oldattr);
  newattr = *oldattr;
  newattr.c_lflag &= ~(ICANON | ECHO | ECHONL | ISIG);
  newattr.c_iflag
    &= ~(IGNBRK | BRKINT | PARMRK | ISTRIP | INLCR | IGNCR | ICRNL | IXON);

  tcsetattr (STDIN_FILENO, TCSANOW, &newattr);

  return oldattr;
}

void restore_echo (struct termios *oldattr)
{
  tcsetattr (STDIN_FILENO, TCSANOW, oldattr);

  free(oldattr);
}
