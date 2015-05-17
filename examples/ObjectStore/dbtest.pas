{$mode objfpc}{$h+}{$M+}
program test;

uses sqlite3, strings, classes, model, mydbmod, sysutils;

var
  Db: PSQLite3;
  //Sql: String;
  
  mydata: TMyData; //the persistable object inherits from TModel
  founddata: TMyData; //a single retrieved object from the database
  resultdata: TList; //should this be a TList specific for TModel?
  i: integer; //just another counter
begin

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
  TMyData(resultdata[5]).Delete(); //first we delete the item from the database
  resultdata.delete(5); //next we delete the item from the list
  resultdata.pack(); //and we clean up the list

  //resultdata.components.add(founddata); //nah this cannot work

  //browse again trough the results now that one has been removed
  for i:=0 to resultdata.count-1 do
    writeln(inttostr(i)+' '+TMyData(resultdata[i]).name);  
  
  //clean up memory
  resultdata.free;
  founddata.free;
  mydata.free;
   
  //close the db connection
  //TODO: should be added to TModel
  sqlite3_close(Db);

  UnLoadSQLite3(); //unload dynamic loaded sqlite3 lib
  readln();
end.
