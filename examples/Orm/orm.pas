unit orm;

{$mode objfpc}{$H+}

//This Source Code Form is subject to the terms of the Mozilla Public
//License, v. 2.0. If a copy of the MPL was not distributed with this
//file, You can obtain one at http://mozilla.org/MPL/2.0/.

interface

uses
  Classes, SysUtils, TypInfo, sqlite3, model;

type
  TOrm= class
  private
    fdatabasename: string;
    fdb: PSqlite3;
  public
    constructor Create(adatabasename: string);
    destructor Destroy(); override;
    procedure Add(aClass: TClass);
    procedure Delete(aModel: Tmodel);
    procedure Save(aModel: TModel);
    function FindOne(aClass: TClass): Tmodel;
    function Find(aClass: TClass; aId: String): TList;
    property DatabaseName: string read fdatabasename write fdatabasename;
  end;

implementation

uses variants;

constructor TOrm.Create(adatabasename: string);
begin
  fdatabasename:=adatabasename;
  fdb:=nil;
  sqlite3_open(pchar(fdatabasename), fdb);
end;

destructor TOrm.Destroy();
begin
  sqlite3_close(fdb);
  fdb:=nil;
  inherited;
end;

procedure TOrm.Add(aClass: TClass);
var
  Sql: string;
  PropCount: integer;
  PropList:  PPropList;
  PropInfo:   PPropInfo;
  i: integer;
begin
  //create a table
  Sql := 'CREATE TABLE IF NOT EXISTS '+aClass.ClassName+' ("uuid" TEXT PRIMARY KEY';

  //add fields
  PropCount := GetPropList(aClass, PropList);
  For i := 0 to PropCount -1 do
  begin;
    PropInfo := PropList^[i];
    if PropInfo^.Name <> 'Tag' then //do nothing with Tag
      begin
        Sql:=Sql+', "'+PropInfo^.Name+'" ';
        case PropInfo^.PropType^.Kind of
          tkInteger,
          tkEnumeration,
          tkSet,
          tkChar,
          tkWChar,
          tkBool,
          tkQWord,
          tkUChar,
          tkInt64: Sql:=Sql+'INTEGER';
          tkFloat: Sql:=Sql+'REAL';
          tkSString,
          tkLString,
          tkAString,
          tkWString,
          tkUString: Sql:=Sql+'TEXT';
          tkClass: Sql:=Sql+'TEXT';
        end;
        Sql:=Sql+' NOT NULL' //hardcoded should contain data
      end;
  end;
  Sql := Sql + ');';
  sqlite3_exec(fdb,pchar(Sql),nil,nil,nil);

  //create a unique index
  Sql := 'CREATE UNIQUE INDEX IF NOT EXISTS "'+aClass.ClassName+'_uuid_index" on '+aClass.ClassName+' (uuid ASC);';
  sqlite3_exec(fdb,pchar(Sql),nil,nil,nil);
end;

//remove the current object from the database
procedure TOrm.Delete(aModel: Tmodel);
var
  Sql: string;
  Stmt: PSQLite3Stmt;
  iStepResult: integer;

begin

  Stmt := nil;
  Sql := 'DELETE FROM '+aModel.ClassName+' WHERE "uuid" == ?;';
  writeln(Sql);

  { Call the query }
  try

    if Sqlite3_Prepare_v2(self.fDB, PAnsiChar(SQL), -1, Stmt, SQLite3_Null) <>
      SQLITE_OK then
      WriteLn('Could not prepare SQL statement'+ SQL);

    if (Stmt = nil) then
      WriteLn('Could not prepare SQL statement'+ SQL);

    sqlite3_bind_text(stmt, 1, PAnsiChar(aModel.Guid), Length(aModel.Guid), nil);

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
procedure TOrm.Save(aModel: TModel);
var
  Sql: String;
  Stmt: PSQLite3Stmt;
  iStepResult: integer;
  aguid: tguid;
  PropCount: integer;
  PropList: PPropList;
  PropInfo: PPropInfo;
  FieldList: String;
  CommaList: String;
  Name: String;
  i: integer;
begin
  //save the record to a record in sqlite table

  FieldList:='';
  CommaList:='';
  Name:='';

  //determine if we have a new record or not
  if aModel.Guid = '' then
  begin
    PropCount := GetPropList(aModel, PropList);
    For i := 0 to PropCount -1 do
      begin;
        PropInfo := PropList^[i];
        if PropInfo^.Name <> 'Tag' then //do nothing with Tag
          begin
            FieldList:=FieldList+', "'+PropInfo^.Name+'"';
            CommaList:=CommaList+', :'+PropInfo^.Name;
          end;
      end;
    Sql := 'INSERT INTO '+aModel.ClassName+' ("uuid"'+FieldList+') VALUES (:guid'+CommaList+');'
  end
  else
  begin
    PropCount := GetPropList(aModel, PropList);
    For i := 0 to PropCount -1 do
      begin;
        PropInfo := PropList^[i];
        if PropInfo^.Name <> 'Tag' then //do nothing with Tag
          begin
            FieldList:=FieldList+', "'+PropInfo^.Name+'" = :'+PropInfo^.Name+' ';
          end;
      end;
    system.delete(FieldList, 1, 2);
    Sql := 'UPDATE '+aModel.ClassName+' SET '+FieldList+' WHERE "uuid" = :guid';
  end;

  Stmt:=nil;
  //call the query with parameters
  try

    //prepare the query so that parameter can be bound to it
    if Sqlite3_Prepare_v2(self.fDB, PAnsiChar(SQL), -1, Stmt, SQLite3_Null) <>
      SQLITE_OK then
      WriteLn('Could not prepare SQL statement'+ SQL);

    if (Stmt = nil) then
      WriteLn('Could not prepare SQL statement'+ SQL);

    //bind the uuid
    if aModel.Guid = '' then
    begin
      createguid(aguid); //for a new record we need to generate a uuid
      aModel.Guid := guidtostring(aguid); //and make it a string
    end;
    sqlite3_bind_text(stmt, sqlite3_bind_parameter_index(stmt, ':guid'), PAnsiChar(aModel.Guid), Length(aModel.Guid), nil);

    //bind the fields
    PropCount := GetPropList(aModel, PropList);

    For i := 0 to PropCount -1 do
      begin;

        PropInfo := PropList^[i];
        if PropInfo^.Name <> 'Tag' then //do nothing with Tag
          begin
            Name:=':'+PropInfo^.Name+#0;
            case PropInfo^.PropType^.Kind of
              tkInteger,
              tkEnumeration,
              tkSet,
              tkChar,
              tkWChar,
              tkBool,
              tkQWord,
              tkUChar: sqlite3_bind_int(stmt, sqlite3_bind_parameter_index(stmt, pansichar(Name)), GetPropValue(AModel,PropInfo^.Name) );
              tkInt64: sqlite3_bind_int64(stmt, sqlite3_bind_parameter_index(stmt, pansichar(Name)), GetPropValue(AModel,PropInfo^.Name) );
              tkFloat: sqlite3_bind_double(stmt, sqlite3_bind_parameter_index(stmt, pansichar(Name)), GetPropValue(AModel,PropInfo^.Name) );
              tkSString,
              tkLString,
              tkAstring: sqlite3_bind_text(stmt, sqlite3_bind_parameter_index(stmt, pansichar(Name)), pchar(vartostr(GetPropValue(AModel,PropInfo^.Name))), Length(GetPropValue(AModel,PropInfo^.Name)), nil);
              tkClass: sqlite3_bind_text(stmt, sqlite3_bind_parameter_index(stmt, pansichar(Name)), pchar(TModel(GetObjectProp(AModel,PropInfo^.Name)).Guid) , length(TModel(GetObjectProp(AModel,PropInfo^.Name)).Guid), nil);
            end;
          end;
      end;

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

//retrieve the first record from the database
function TOrm.FindOne(aClass: TClass): TModel;
var
  found: TList;
  i: integer;
begin
  found:= Find(aClass, '');
  result := TModel(found[0]); //get the first result
  for i:=found.count-1 downto 1 do
  begin
    TModel(found[i]).free;
    found[i]:=nil;
    found.Delete(i);
  end;
  //the caller of findone should free the result
  found.Delete(0);
  freeAndNil(found); //clean up and free some memory
end;

//retrieve records from database



function TOrm.Find(aClass: TClass; aId: String): TList;
var
  sql: string;
  Stmt: PSQLite3Stmt;
  iStepResult: integer;
  tempmodel: TModel;
  PropCount: integer;
  PropList:  PPropList;
  PropInfo:  PPropInfo;
  i: integer;
    NumColumns: integer;
  c: integer;
  oid: string;
  found: TList;
begin

  Stmt := nil;

  result := TList.Create();

  sql := 'SELECT * FROM '+aClass.ClassName;
  if aId<>'' then
    begin
      sql:=sql+' WHERE "uuid" = :guid';
    end;
  sql:=sql+' ;';

  try

    if Sqlite3_Prepare_v2(self.fDB, PAnsiChar(SQL), -1, Stmt, SQLite3_Null) <>
      SQLITE_OK then
      WriteLn('Could not prepare SQL statement'+ SQL);

    if (Stmt = nil) then
      WriteLn('Could not prepare SQL statement'+ SQL);

    if aId<>'' then
      begin
      sqlite3_bind_text(stmt, sqlite3_bind_parameter_index(stmt, ':guid'), PAnsiChar(aId), Length(aId), nil);
      end;

    iStepResult := Sqlite3_step(Stmt);

    while iStepResult = SQLITE_ROW do
    begin

    if ((iStepResult <> SQLITE_DONE) and (iStepResult <> SQLITE_ROW)) then
    begin
      SQLite3_reset(stmt);
      WriteLn('Error executing SQL statement: '+ SQL);
    end else
    begin
      //TODO: retrieve fields
        NumColumns := sqlite3_column_count(stmt);
        tempmodel:=TModelType(GetClass(aClass.ClassName)).Create();
        tempmodel.Guid:=Sqlite3_column_Text(stmt, 0);

        PropCount := GetPropList(aClass, PropList);
        For c := 0 to NumColumns -1 do
        For i := 0 to PropCount -1 do
        begin;
          PropInfo := PropList^[i];
          if PropInfo^.Name <> 'Tag' then //do nothing with Tag
            begin
              Sql:=Sql+', "'+PropInfo^.Name+'" ';
              case PropInfo^.PropType^.Kind of
                tkInteger,
                tkEnumeration,
                tkSet,
                tkChar,
                tkWChar,
                tkBool,
                tkQWord,
                tkUChar: if sqlite3_column_name(stmt,c)=PropInfo^.Name then SetPropValue(tempmodel, PropInfo,sqlite3_column_int(stmt, c));
                tkInt64: if sqlite3_column_name(stmt,c)=PropInfo^.Name then SetPropValue(tempmodel, PropInfo,sqlite3_column_int64(stmt, c));
                tkFloat: if sqlite3_column_name(stmt,c)=PropInfo^.Name then SetPropValue(tempmodel, PropInfo,sqlite3_column_double(stmt, c));
                tkSString,
                tkLString,
                tkAString,
                tkWString,
                tkUString: if sqlite3_column_name(stmt,c)=PropInfo^.Name then SetPropValue(tempmodel, PropInfo, sqlite3_column_text(stmt, c));
                tkClass: if sqlite3_column_name(stmt,c)=PropInfo^.Name then
                  begin
                    oid := sqlite3_column_text(stmt, c);
                    found :=Find(GetClass(PropInfo^.PropType^.Name),oid);
                    SetObjectProp(tempModel,PropInfo^.Name,Tmodel(found[0]));
                end;
              end;
              Sql:=Sql+' NOT NULL' //hardcoded should contain data
            end;
        end;


      result.Add(tempmodel);
    end;
    iStepResult := Sqlite3_step(Stmt);
    end;

  finally

    if Assigned(Stmt) then
      Sqlite3_Finalize(stmt);

  end;
end;

end.

