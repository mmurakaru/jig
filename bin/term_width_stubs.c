/* Terminal size via ioctl; 0 when stdout is not a terminal. */
#include <caml/mlvalues.h>
#include <sys/ioctl.h>
#include <unistd.h>

CAMLprim value jig_terminal_columns(value unit)
{
    struct winsize ws;
    (void)unit;
    if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws) == 0 && ws.ws_col > 0)
        return Val_int(ws.ws_col);
    return Val_int(0);
}

CAMLprim value jig_terminal_rows(value unit)
{
    struct winsize ws;
    (void)unit;
    if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws) == 0 && ws.ws_row > 0)
        return Val_int(ws.ws_row);
    return Val_int(0);
}
