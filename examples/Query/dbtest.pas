program dbtest;

// Any copyright is dedicated to the Public Domain.
// http://creativecommons.org/publicdomain/zero/1.0/

{$MODE DELPHI}

uses sqlite3, sysutils;

const
 DBFILE='dbtest.db';

var
 rc       : Integer;
 db       : Psqlite3;
 sql      : string;
 pzErrMsg : PChar;
 
function MyCallback(_para1:pointer; plArgc:longint; argv:PPchar; argcol:PPchar):longint; cdecl;
var i: Integer;
    PVal, PName: ^PChar;
begin
 PVal:=argv;
 PName:=argcol;
 for i:=0 to plArgc-1 do begin
  writeln(Format('%s = ''%s'''#13, [PName^, PVal^]));
  inc(PVal);
  inc(PName);
 end;
 writeln(#13);
 Result:=0;
end;

begin

  {$IFDEF DEBUG}
  // Assuming your build mode sets -dDEBUG in Project Options/Other when defining -gh
  // This avoids interference when running a production/default build without -gh

  // Set up -gh output for the Leakview package:
  if FileExists('heap.trc') then
     DeleteFile('heap.trc');
  SetHeapTraceOutput('heap.trc');
  {$ENDIF DEBUG}

  writeln('init');
  LoadSqlite3();

  writeln('sqlite test');
  db:=nil;
  rc := sqlite3_open(PChar(DBFILE), db);
  try
   if rc<>SQLITE_OK then begin
    writeln(Format('Can''t open database: %s',[DBFILE]));
   end;

   writeln('set encryption');
   sqlite3_key(db,'test', 4); //encryption support

   (*
   writeln('drop table test');
   sql:= 'DROP TABLE Test;';
   rc:=sqlite3_exec(db, PChar(sql), @MyCallback, nil, @pzErrMsg);
   if( rc<>SQLITE_OK )
   then writeln(Format('SQL error: %s', [pzErrMsg^]));

   writeln('create table test');
   sql:='CREATE TABLE Test(No integer, name varchar(32),shortname varchar(32), age integer);';
   rc:=sqlite3_exec(db, PChar(sql), @MyCallback, nil, @pzErrMsg);
   if( rc<>SQLITE_OK )
   then writeln(Format('SQL error: %s', [pzErrMsg^]));
   
   writeln('insert into table test #1');
   sql:='INSERT INTO Test VALUES(1,''hi'', ''by'', -1);';
   rc:=sqlite3_exec(db, PChar(sql), @MyCallback, nil, @pzErrMsg);
   Writeln('Inserting row');
   if( rc<>SQLITE_OK )
   then writeln(Format('SQL error: %s', [pzErrMsg^]));

   writeln('insert into table test #2');
   SQL := 'INSERT INTO Test VALUES(2,''dualcore'', ''runwell'',-1);';
   rc:=sqlite3_exec(db, PChar(sql), @MyCallback, nil, @pzErrMsg);
   Writeln('Inserting row') ;
   if( rc<>SQLITE_OK )
   then writeln(Format('SQL error: %s', [pzErrMsg^]));

   writeln('insert into table test #3');
   SQL := 'INSERT INTO Test VALUES(3,''Hello'', ''World'',NULL);';
   rc:=sqlite3_exec(db, PChar(sql), @MyCallback, nil, @pzErrMsg);
   Writeln('Inserting row') ;
   if( rc<>SQLITE_OK )
   then writeln(Format('SQL error: %s', [pzErrMsg^]));

   writeln('insert into table test #4');
   SQL := 'INSERT INTO Test VALUES(4,''just a little'', ''test'',-1);';
   rc:=sqlite3_exec(db, PChar(sql), @MyCallback, nil, @pzErrMsg);
   Writeln('Inserting row') ;
   if( rc<>SQLITE_OK )
   then writeln(Format('SQL error: %s', [pzErrMsg^]));
   *)

   writeln('select table test');
   SQL := 'select * from Test;';
   rc:=sqlite3_exec(db, PChar(sql), @MyCallback, nil, @pzErrMsg);
   if( rc<>SQLITE_OK )
   then writeln(Format('SQL error: %s', [pzErrMsg^]));

   writeln('close database');
  finally sqlite3_close(db); end;

  writeln('wait 50000');
  sleep(5000);

  writeln('deinit');
  UnLoadSqlite3();
end.
