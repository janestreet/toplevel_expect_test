module Toploop = Toploop
module Topdirs = Topdirs

module Language_extension = struct
  let enable_of_string_exn _ = failwith "Compiler extensions are not supported."
end
