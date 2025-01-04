{$mode objfpc}{$h+}{$M+}
program test;

// Any copyright is dedicated to the Public Domain.
// http://creativecommons.org/publicdomain/zero/1.0/

uses sqlite3, strings, classes, model, mydbmod, sysutils;

var
  Db: PSQLite3;
  //Sql: String;
  
  mydata: TMyData; //the persistable object inherits from TModel
  founddata: TMyData; //a single retrieved object from the database
  resultdata: TList; //should this be a TList specific for TModel?
  i: integer; //just another counter
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
  //open a database connection
  //TODO: should be added to TModel
  Db:=nil;
  sqlite3_open(pchar('test.db'), Db);

  mydata := TMyData.Create(nil);
  
  //prepare
  mydata.Db := Db; //pass on the database connection
  mydata.TableName := 'mydata'; //specify a tablename (could this be classname?)
  mydata.init(); //prepare database if not already done yet

  //make record
  mydata.Id := 1;
  mydata.Name := 'hallo wereld';
  mydata.save(); //persist it in the database

  //search first record
  founddata := mydata.findone() as TMyData;
  writeln(founddata.Name);

  //change the contents
  founddata.name := 'xx hello :-)'; 
  founddata.save(); //and save it again
  
  //change the contents again but dont persist it
  founddata.name := 'zz'; //so this change will be lost

  //retrieve all records
  resultdata := mydata.find(); //TODO: add filters to find specific records

  //browse trough the results
  for i:=0 to resultdata.count-1 do
    writeln(inttostr(i)+' '+TMyData(resultdata[i]).name);
  
  //remove record from database
  if resultdata.count>5 then
     begin
      TMyData(resultdata[5]).Delete(); //first we delete the item from the database
      TmyData(resultdata[5]).Free;
      resultdata[5]:=nil;
      resultdata.delete(5); //next we delete the item from the list
      resultdata.pack(); //and we clean up the list
     end;
  //resultdata.components.add(founddata); //nah this cannot work

  //browse again trough the results now that one has been removed
  for i:=0 to resultdata.count-1 do
    writeln(inttostr(i)+' '+TMyData(resultdata[i]).name);  


  //close the db connection
  //TODO: should be added to TModel
  sqlite3_close(Db);

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

  readln();
end.
