ok() {
   exec 3>&1
   eval `
      exec 4>&1 >&3 3>&-
      {
          eval "$@" 2>&1 >/dev/null
          echo "EC=$?;" >&4
      } | sed 's/^/# /'
      `
   [ "$EC" = 0 ] && echo "ok - $@" || echo "not ok - $@: $EC"
   return $EC
}


# example usage:

 # ok true
 # ok ! false
 # 
 # ok "(ok true | grep -q '^ok')"
 # ok "(ok false | grep -q '^not ok')"
 # 
 # # self-test
 # ok ok true
 # ok ! ok "false"
 # 
 # ok "(ok 'echo foo>/dev/stderr' 2>&1 | grep -q '# foo') # stderr gets commented"