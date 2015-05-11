
type serial =
  { fd : Lwt_unix.file_descr;
    input : Lwt_io.input_channel;
    output : Lwt_io.output_channel }

let default_baudrate = 57600

let open_serial ?(baudrate=default_baudrate) ~path =
  lwt fd = Lwt_unix.openfile path [Unix.O_RDWR; Unix.O_NONBLOCK] 0o660 in
  let tio = {
    (* taken from minicom *)
    Unix.c_ignbrk = true; Unix.c_brkint = false; Unix.c_ignpar = false;
    Unix.c_parmrk = false; Unix.c_inpck = false; Unix.c_istrip = false;
    Unix.c_inlcr = false; Unix.c_igncr = false; Unix.c_icrnl = false;
    Unix.c_ixon = false; Unix.c_ixoff = false; Unix.c_opost = false;
    Unix.c_obaud = baudrate; Unix.c_ibaud = baudrate; Unix.c_csize = 8;
    Unix.c_cstopb = 1; Unix.c_cread = true; Unix.c_parenb = false;
    Unix.c_parodd = false; Unix.c_hupcl = false; Unix.c_clocal = true;
    Unix.c_isig = false; Unix.c_icanon = false; Unix.c_noflsh = false;
    Unix.c_echo = false; Unix.c_echoe = false; Unix.c_echok = false;
    Unix.c_echonl = false; Unix.c_vintr = '\000'; Unix.c_vquit = '\000';
    Unix.c_verase = '\000'; Unix.c_vkill = '\000'; Unix.c_veof = '\000';
    Unix.c_veol = '\000'; Unix.c_vmin = 1; Unix.c_vtime = 5;
    Unix.c_vstart = '\000'; Unix.c_vstop = '\000'
  } in
  lwt () = Lwt_unix.tcsetattr fd Unix.TCSAFLUSH tio in
  let input = Lwt_io.of_fd ~mode:Lwt_io.input fd in
  let output = Lwt_io.of_fd ~mode:Lwt_io.output fd in
  Lwt.return { fd; input; output }

let read_line s = Lwt_io.read_line s.input
let write_line s msg = Lwt_io.write_line s.output msg

let rec read_string ~max_length serial =
  let s = Bytes.create max_length in
  lwt c = Lwt_unix.read serial.fd s 0 (Bytes.length s) in
  if c = 0
  then
    lwt () = Lwt_unix.sleep 0.1 in
    read_string ~max_length serial
  else
    Lwt.return (String.sub s 0 c)

let send_string serial s =
  let rec iter n =
    lwt c = Lwt_unix.write serial.fd s n (String.length s - n) in
    if c + n < String.length s
    then
      lwt () =
        if c = 0
        then Lwt_unix.sleep 0.1
        else Lwt.return () in
      iter (n + c)
    else Lwt.return ()
  in
  lwt () = iter 0 in
  Lwt.return ()
