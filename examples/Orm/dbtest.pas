{$mode objfpc}{$h+}{$M+}
program test;

// Any copyright is dedicated to the Public Domain.
// http://creativecommons.org/publicdomain/zero/1.0/

uses sqlite3, strings, classes, model, mydbmod, sysutils, orm;

var
  mydata: TMyData; //the persistable object inherits from TModel
  founddata: TMyData; //a single retrieved object from the database
  resultdata: TList; //should this be a TList specific for TModel?
  mynote: TNote;
  readnote: TNote;
  myauthor: TAuthor;
  i: integer; //just another counter
  myOrm : Torm;
begin

  {$IFDEF DEBUG}
  // Assuming your build mode sets -dDEBUG in Project Options/Other when defining -gh
  // This avoids interference when running a production/default build without -gh

  // Set up -gh output for the Leakview package:
  if FileExists('heap.trc') then
     DeleteFile('heap.trc');
  SetHeapTraceOutput('heap.trc');
  {$ENDIF DEBUG}



  LoadSQLite3(); //dynamicly load sqlite3 lib

  writeln('Init database test.db');
  myOrm := TOrm.Create('test.db');

  writeln('Add model TMyData');
  myOrm.Add(TMyData);

  writeln('Add model TAuthor');
  myOrm.Add(TAuthor);
  myauthor := TAuthor.Create();

  writeln('Add model TNote');
  myOrm.Add(TNote);
  mynote := TNote.Create();

  //make mydata record
  mydata := TMyData.Create();
  mydata.Id := 1;
  mydata.Name := 'hallo wereld';
  myOrm.save(myData); //persist it in the database
  writeln('Saved myData');

  //make myauthor record
  myauthor := TAuthor.Create();
  myauthor.Name := 'John Doe';
  myOrm.save(myauthor); //persist it in the database
  writeln('Saved myauthor');

  //make mynote record
  mynote := TNote.Create();
  mynote.Title := 'Hello World';
  mynote.Text := 'Hello World this is John Doe writing.';
  mynote.Author:=myAuthor;
  myOrm.save(mynote); //persist it in the database
  writeln('Saved mynote');


  //search first record
  founddata := myOrm.findone(TMyData) as TMyData;
  writeln(founddata.Name);

  //change the contents
  founddata.name := 'xx hello :-)';
  myOrm.save(founddata); //and save it again
  
  //change the contents again but dont persist it
  founddata.name := 'zz'; //so this change will be lost

  //retrieve all records
  resultdata := myOrm.find(TMyData,''); //TODO: add filters to find specific records

  //browse trough the results
  for i:=0 to resultdata.count-1 do
    writeln(inttostr(i)+' '+TMyData(resultdata[i]).name);
  
  //remove record from database
  if resultdata.count>5 then
     begin
      myOrm.Delete(TMyData(resultdata[5]));//first we delete the item from the database
      TmyData(resultdata[5]).Free;
      resultdata[5]:=nil;
      resultdata.delete(5); //next we delete the item from the list
      resultdata.pack(); //and we clean up the list
     end;

  //browse again trough the results now that one has been removed
  for i:=0 to resultdata.count-1 do
    writeln(inttostr(i)+' '+TMyData(resultdata[i]).name);

  readNote := myOrm.findone(TNote) as TNote;
  writeln(readNote.Title);
  writeln(readNote.Author.Name);

  myOrm.Free;

  UnLoadSQLite3(); //unload dynamic loaded sqlite3 lib

  //clean up memory
  for i:=resultdata.Count-1 downto 0 do
    begin
      TModel(resultdata[i]).Free();
      resultdata[i]:=nil;
      resultdata.Delete(i);
    end;
  freeAndNil(resultdata);

  freeAndNil(founddata);

  freeAndNil(mydata);

  freeAndNil(mynote);
  freeAndNil(myauthor);

  readln();
end.
