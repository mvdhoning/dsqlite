{$mode objfpc}{$h+}{$M+} 
unit model;

interface

uses sqlite3, classes, sysutils;

type
  
  TModel = class(TComponent)
  private
    ftablename: string;
    fdb: PSqlite3;
    fguid: string;
  public
    procedure Init();
    function FindOne(): TModel;
    function Find(): TList;
    procedure Save();
    procedure Delete();
    property TableName: String read ftablename write ftablename;
    property Db: PSqlite3 read fdb write fdb;
    property Guid: string read fguid write fguid;
  end;
  
procedure DisposePointer(ptr: pointer); cdecl;

implementation

//TODO: find a way to not need this
procedure DisposePointer(ptr: pointer); cdecl;
begin
  if assigned(ptr) then
    freemem(ptr);
end;

//prepare the database for this class if not already done so
procedure TModel.Init();
var
  Sql: String;
begin
  //create a table
  Sql := 'CREATE TABLE IF NOT EXISTS '+ftablename+' ( "id" INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL, "uuid" TEXT NOT NULL, "data" BLOB NOT NULL);';
  sqlite3_exec(db,pchar(Sql),nil,nil,nil);

  //create a unique index
  Sql := 'CREATE UNIQUE INDEX IF NOT EXISTS "'+ftablename+'_uuid_index" on '+ftablename+' (uuid ASC);';
  sqlite3_exec(db,pchar(Sql),nil,nil,nil);
  
  //TODO: create additional indices
  
end;

//remove the current object from the database
procedure TModel.Delete();
var
  Sql: string;
  Stmt: PSQLite3Stmt;
  iStepResult: integer;
  test: pchar;

begin
  test:=nil;
  Sql := 'DELETE FROM '+ftablename+' WHERE "uuid" == ?;';
  writeln(Sql);

  { Call the query }
  try

    if Sqlite3_Prepare_v2(self.fDB, PAnsiChar(SQL), -1, Stmt, test) <>
      SQLITE_OK then
      WriteLn('Could not prepare SQL statement'+ SQL);

    if (Stmt = nil) then
      WriteLn('Could not prepare SQL statement'+ SQL);

    writeln(fguid);      
    sqlite3_bind_text(stmt, 1, PAnsiChar(fguid), Length(fguid), nil);

    iStepResult := Sqlite3_step(Stmt);

    if (iStepResult <> SQLITE_DONE) then
      begin
      SQLite3_reset(stmt);
      WriteLn('Error executing SQL statement: '+ SQL);
      end;

  finally

    if Assigned(Stmt) then
      Sqlite3_Finalize(stmt);
      
  end;


end;

//persist the current object to the database
procedure TModel.Save();
var
  Sql: String;
  AStream: TMemoryStream;
  Stmt: PSQLite3Stmt;
  iStepResult: integer;
  iBindResult: integer;
  iSize: integer;
  ptr: pointer;
  aguid: tguid;
  test: pchar;
begin
    test:=nil;
  //save the record to a stream
  AStream := TMemoryStream.Create();
  AStream.writecomponent(self);
  
  //determine if we have a new record or not
  if fguid = '' then
    Sql := 'INSERT INTO '+ftablename+' ("data", "uuid") VALUES (?, ?);'
  else
    Sql := 'UPDATE '+ftablename+' SET "data" = ? WHERE "uuid" = ?';
  
  writeln(Sql);
  
  //call the query with parameters
  try

    //prepare the query so that parameter can be bound to it
    if Sqlite3_Prepare_v2(self.fDB, PAnsiChar(SQL), -1, Stmt, test) <>
      SQLITE_OK then
      WriteLn('Could not prepare SQL statement'+ SQL);

    if (Stmt = nil) then
      WriteLn('Could not prepare SQL statement'+ SQL);
    
    //bind the uuid
    if fguid = '' then
    begin
      createguid(aguid); //for a new record we need to generate a uuid
      fguid := guidtostring(aguid); //and make it a string
    end;
    writeln(fguid);      
    sqlite3_bind_text(stmt, 2, PAnsiChar(fguid), Length(fguid), nil);

    //now bind the blob data
    iSize := AStream.size;
    GetMem(ptr, iSize);
    if (ptr = nil) then
      Writeln('Leeg');
    AStream.position := 0;
    AStream.Read(ptr^, iSize);
    iBindResult := SQLite3_Bind_Blob(stmt, 1, ptr, iSize, @DisposePointer);
    if iBindResult <> SQLITE_OK then
      WriteLn('Error binding blob to database'+ SQL);

    //execute the query with bound parameters
    iStepResult := Sqlite3_step(Stmt);

    if (iStepResult <> SQLITE_DONE) then
      begin
      SQLite3_reset(stmt);
      WriteLn('Error executing SQL statement: '+ SQL);
      end;

  finally

    if Assigned(Stmt) then
      Sqlite3_Finalize(stmt);
      
  end;
    
end;

//retrieve the first object
function TModel.FindOne(): TModel;
var
  found: TList;
begin
  found:= Find();
  result := TModel(found[0]); //get the first result
  found.free; //clean up and free some memory
end;

//retrieve objects from database
function TModel.Find(): TList;
var
  sql: string;
  AStream: Tmemorystream;
  Stmt: PSQLite3Stmt;
  iStepResult: integer;
  iSize: integer;
  ptr: pointer;
  tempmodel: TModel;
  test: pchar;
begin
    test:=nil;
  result := TList.Create();
                  
  sql := 'SELECT * FROM '+ftablename+';';
  
  try
  
    if Sqlite3_Prepare_v2(self.fDB, PAnsiChar(SQL), -1, Stmt, test) <>
      SQLITE_OK then
      WriteLn('Could not prepare SQL statement'+ SQL);

    if (Stmt = nil) then
      WriteLn('Could not prepare SQL statement'+ SQL);

    iStepResult := Sqlite3_step(Stmt);

    while iStepResult = SQLITE_ROW do
    begin

    if ((iStepResult <> SQLITE_DONE) and (iStepResult <> SQLITE_ROW)) then
    begin
      SQLite3_reset(stmt);
      WriteLn('Error executing SQL statement: '+ SQL);
    end else 
    begin
      //retrieve the blob from the database and make an object from it
      AStream := TMemoryStream.Create();
      ptr := sqlite3_column_blob(stmt, 2);
      iSize := sqlite3_column_bytes(Stmt, 2);
      AStream.Write(ptr^,iSize);
      AStream.position := 0;
      //enrich the 'new' model with metadata      
      tempmodel := AStream.readcomponent(nil) as tmodel;
      tempmodel.guid := Sqlite3_column_Text(stmt, 1);
      tempmodel.TableName := self.tablename;
      tempmodel.Db := self.fdb;
      //add the object to the results      
      result.Add(tempmodel);
      //clean up
      AStream.free();       
    end;
    iStepResult := Sqlite3_step(Stmt);
    end;

  finally

    if Assigned(Stmt) then
      Sqlite3_Finalize(stmt);
      
  end;  
end;

end.
