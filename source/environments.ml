open Environment

class environments =
  let environments = Hashtbl.create 0 in
  let initialEnvironment = new environment() in
  object(self)
    method get(index) = Hashtbl.find environments index
    method get_variable(index, variable) = (self#get(index))#get(variable)
    method update(index, environment) = Hashtbl.replace environments index environment
    method to_list = Hashtbl.fold (fun index environment accumulator -> (index, environment)::accumulator) environments []
    initializer
      self#update(0, initialEnvironment)
  end