include Option

module Let_syntax = struct
  module Let_syntax = struct
    let return (o : 'a) : 'a option = Some o

    let map ~(f : 'a -> 'b) (o : 'a option) : 'b option = Option.map f o

    let bind (o : 'a option) ~(f : 'a -> 'b option) : 'b option =
      Option.bind o f
  end
end
