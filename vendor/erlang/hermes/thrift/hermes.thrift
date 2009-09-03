typedef string Cloudname
typedef string Meth
typedef string Data

struct RunOnCloud {
  1:Cloudname name
  2:Meth method
  3:list<Data> results
}

exception FailureException {
  1:string message
}


service Hermes {
  RunOnCloud run(1:Cloudname name, 2:Meth method, 3:Data results) throws (1:FailureException fail)
}