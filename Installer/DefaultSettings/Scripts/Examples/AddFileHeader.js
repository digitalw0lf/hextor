// Insert data from specified file at the beginning of all open files

var HeaderFile = "c:\\Header.txt";
for (var i=0; i<EditorCount; i++) {
  Editors(i).InsertDataFromFile(HeaderFile, 0, false);
}
