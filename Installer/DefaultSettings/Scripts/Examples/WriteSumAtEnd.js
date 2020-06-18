// Calculate summ of all bytes of current file
// and write it at the end of file

var sum = 0;
for (var i=0; i<ActiveEditor.Data.GetSize(); i++) {
  sum += ActiveEditor.Data.Get(i); 
}
ActiveEditor.Data.Change(ActiveEditor.Data.GetSize(), sum);