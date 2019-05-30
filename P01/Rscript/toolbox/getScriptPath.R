getScriptDir = function(){
  # Get the directory of executing script
  dirname(sys.frame(1)$ofile)
}