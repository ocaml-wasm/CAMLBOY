open Camlboy_lib

let run_rom_bytes rom_bytes frames =
  let cartridge = Detect_cartridge.f ~rom_bytes in
  let module C = Camlboy.Make(val cartridge) in
  let t =  C.create_with_rom ~print_serial_port:false ~rom_bytes in
  let frame_count = ref 0 in
  let start_time = ref Brr.(Performance.now_ms G.performance) in
  while !frame_count < frames do
    match C.run_instruction t with
    | In_frame -> ()
    | Frame_ended _ -> incr frame_count
  done;
  Brr.(Performance.now_ms G.performance) -. !start_time

let run_rom_path rom_path frames =
  let s =
    In_channel.(with_open_bin ("../../resource/games/" ^ rom_path) input_all) in
  let rom = Bigstringaf.of_string ~off:0 ~len:(String.length s) s in
  run_rom_bytes rom frames

let main () =
  (* Read URL parameters *)
  let rom_path = "tobu.gb" in
  let frames =
    if Array.length Sys.argv > 1 then int_of_string Sys.argv.(1) else 15000 in
  (* Load initial rom *)
  let duration_ms = run_rom_path rom_path frames in
  let duration = duration_ms /. 1000. in
  let fps = Float.(of_int frames /. duration) in
  Printf.printf "%f\n" fps

let () = main ()
