namespace rb CloudThrift
namespace erl cloudthrift
namespace py cloudthrift

struct CloudQuery {
  1: string name
}

struct CloudResponse {
  1: string name
  2: string command
  3: list<string> response
}

service CommandInterface {
  CloudResponse run_command(1:CloudQuery cld, 2:string command, 3:list<string> arglist)
}