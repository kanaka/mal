(* Moscow ML does not have the LargeInt structure,
 * but its Int is 64 bit on 64 bit systems.
 * We need 64 bit integers for the `time-ms` core function.
 *)

structure LargeInt = Int
