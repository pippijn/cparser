let (|>) a f = f a
let (|.) f g = fun x -> g (f x)
