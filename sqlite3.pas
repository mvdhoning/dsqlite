unit SQLite3;

//This Source Code Form is subject to the terms of the Mozilla Public
//License, v. 2.0. If a copy of the MPL was not distributed with this
//file, You can obtain one at http://mozilla.org/MPL/2.0/.

//  Pascal unit with dynamic loading of SQLite3 lib by M van der Honing

{$IFDEF FPC}
  {$MODE DELPHI}
  {$IFNDEF WINDOWS}
    {$LINKLIB c}
  {$ENDIF}
  {$PACKENUM 4}    (* use 4-byte enums *)
  {$PACKRECORDS C} (* C/C++-compatible record packing *)
{$ELSE}
  {$MINENUMSIZE 4} (* use 4-byte enums *)
{$ENDIF}

interface

const
{$IF Defined(WINDOWS)}
  SQLITE3_NAME = 'sqlite3.dll';
{$ELSEIF Defined(UNIX)}
  SQLITE3_NAME = 'sqlite3.so';
{$IFEND}

//Dynamic loading and unloading of Lua libs
function loadSqlite3: boolean;
function loadSqlite3From(FileName: string): boolean;
procedure unLoadSqlite3;
function isSqlite3Loaded: boolean;

const
  // Return values for sqlite3_exec() and sqlite3_step()
  SQLITE_OK          =  0; // Successful result
  // beginning-of-error-codes
  SQLITE_ERROR       =  1; // SQL error or missing database
  SQLITE_INTERNAL    =  2; // An internal logic error in SQLite
  SQLITE_PERM        =  3; // Access permission denied
  SQLITE_ABORT       =  4; // Callback routine requested an abort
  SQLITE_BUSY        =  5; // The database file is locked
  SQLITE_LOCKED      =  6; // A table in the database is locked
  SQLITE_NOMEM       =  7; // A malloc() failed
  SQLITE_READONLY    =  8; // Attempt to write a readonly database
  SQLITE_INTERRUPT   =  9; // Operation terminated by sqlite3_interrupt()
  SQLITE_IOERR       = 10; // Some kind of disk I/O error occurred
  SQLITE_CORRUPT     = 11; // The database disk image is malformed
  SQLITE_NOTFOUND    = 12; // (Internal Only) Table or record not found
  SQLITE_FULL        = 13; // Insertion failed because database is full
  SQLITE_CANTOPEN    = 14; // Unable to open the database file
  SQLITE_PROTOCOL    = 15; // Database lock protocol error
  SQLITE_EMPTY       = 16; // Database is empty
  SQLITE_SCHEMA      = 17; // The database schema changed
  SQLITE_TOOBIG      = 18; // Too much data for one row of a table
  SQLITE_CONSTRAINT  = 19; // Abort due to contraint violation
  SQLITE_MISMATCH    = 20; // Data type mismatch
  SQLITE_MISUSE      = 21; // Library used incorrectly
  SQLITE_NOLFS       = 22; // Uses OS features not supported on host
  SQLITE_AUTH        = 23; // Authorization denied
  SQLITE_FORMAT      = 24; // Auxiliary database format error
  SQLITE_RANGE       = 25; // 2nd parameter to sqlite3_bind out of range
  SQLITE_NOTADB      = 26; // File opened that is not a database file
  SQLITE_ROW         = 100; // sqlite3_step() has another row ready
  SQLITE_DONE        = 101; // sqlite3_step() has finished executing

  SQLITE_INTEGER = 1;
  SQLITE_FLOAT   = 2;
  SQLITE_TEXT    = 3;
  SQLITE_BLOB    = 4;
  SQLITE_NULL    = 5;

  SQLITE_UTF8     = 1;
  SQLITE_UTF16    = 2;
  SQLITE_UTF16BE  = 3;
  SQLITE_UTF16LE  = 4;
  SQLITE_ANY      = 5;

  SQLITE_STATIC    = Pointer(0);
  SQLITE_TRANSIENT = Pointer(-1);

type
  PPAnsiCharArray = ^TPAnsiCharArray;
  TPAnsiCharArray = array[0..MaxInt div SizeOf(PAnsiChar) - 1] of PAnsiChar;

  PSQLite3 = type Pointer;
  PSQLite3Result = PPAnsiCharArray;
  PSQLite3Stmt = type Pointer;
  PSQLite3Backup = type pointer;

  TSQLiteExecCallback = function(UserData: Pointer; NumCols: integer; ColValues: PPCharArray; ColNames: PPCharArray): integer; cdecl;
  TSQLiteBusyHandlerCallback = function(UserData: Pointer; P2: integer): integer; cdecl;
  TSQLite3Destructor = procedure(Ptr: Pointer); cdecl;
  TCollateXCompare = function(UserData: pointer; Buf1Len: integer; Buf1: pointer; Buf2Len: integer; Buf2: pointer): integer; cdecl;

var
  SQLite3_Null : pchar = nil;

  SQLite3_Open : function (filename: PAnsiChar; var db: PSQLite3): integer; cdecl;
  SQLite3_Open16 : function (filename: PChar; var db: PSQLite3): integer; cdecl;
  SQLite3_Close : function (db: PSQLite3): integer; cdecl;
  SQLite3_Exec : function (db: PSQLite3; const SQLStatement: PAnsiChar; CallbackPtr: TSQLiteExecCallback; UserData: Pointer; ErrMsg: PChar): integer; cdecl;
  SQLite3_Version : function (): PAnsiChar; cdecl;
  SQLite3_ErrMsg : function (db: PSQLite3): PAnsiChar; cdecl;
  SQLite3_ErrCode : function (db: PSQLite3): integer; cdecl;
  SQlite3_Free : procedure (P: pointer); cdecl;
  SQLite3_GetTable : function (db: PSQLite3; SQLStatement: PAnsiChar; var ResultPtr: PSQLite3Result; var RowCount: Cardinal; var ColCount: Cardinal; var ErrMsg: PChar): integer; cdecl;
  SQLite3_FreeTable : procedure (Table: PSQLite3Result); cdecl;
  SQLite3_Complete : function (P: PAnsiChar): boolean; cdecl;
  SQLite3_LastInsertRowID : function (db: PSQLite3): int64; cdecl;
  SQLite3_Interrupt : procedure (db: PSQLite3); cdecl;
  SQLite3_BusyHandler : procedure(db: PSQLite3; CallbackPtr: TSQLiteBusyHandlerCallback; UserData: Pointer); cdecl;
  SQLite3_BusyTimeout : procedure(db: PSQLite3; TimeOut: integer); cdecl;
  SQLite3_Changes : function (db: PSQLite3): integer; cdecl;
  SQLite3_TotalChanges : function (db: PSQLite3): integer; cdecl;
  SQLite3_Prepare : function (db: PSQLite3; SQLStatement: PAnsiChar; nBytes: integer; var hStmt: PSQLite3Stmt; var pzTail: PChar): integer; cdecl;
  SQLite3_Prepare_v2 : function (db: PSQLite3; SQLStatement: PAnsiChar; nBytes: integer; var hStmt: PSQLite3Stmt; var pzTail: PChar): integer; cdecl;
  SQLite3_Prepare16 : function (db: PSQLite3; SQLStatement: PChar; nBytes: integer; var hStmt: PSQLite3Stmt; var pzTail: PChar): integer; cdecl;
  SQLite3_Prepare16_v2 : function (db: PSQLite3; SQLStatement: PChar; nBytes: integer; var hStmt: PSQLite3Stmt; var pzTail: PChar): integer; cdecl;
  SQLite3_Column_Count : function (hStmt: PSQLite3Stmt): integer; cdecl;
  SQLite3_Column_Name : function (hStmt: PSQLite3Stmt; ColNum: integer): PAnsiChar; cdecl;
  SQLite3_Column_Name16 : function (hStmt: PSQLite3Stmt; ColNum: integer): PChar; cdecl;
  SQLite3_Column_DeclType : function (hStmt: PSQLite3Stmt; ColNum: integer): PAnsiChar; cdecl;
  SQLite3_Column_DeclType16 : function (hStmt: PSQLite3Stmt; ColNum: integer): PChar; cdecl;
  SQLite3_Step : function (hStmt: PSQLite3Stmt): integer; cdecl;
  SQLite3_DataCount : function (hStmt: PSQLite3Stmt): integer; cdecl;
  SQLite3_Column_Blob : function (hStmt: PSQLite3Stmt; ColNum: integer): pointer; cdecl;
  SQLite3_Column_Bytes : function (hStmt: PSQLite3Stmt; ColNum: integer): integer; cdecl;
  SQLite3_Column_Bytes16 : function (hStmt: PSQLite3Stmt; ColNum: integer): integer; cdecl;
  SQLite3_Column_Double : function (hStmt: PSQLite3Stmt; ColNum: integer): double; cdecl;
  SQLite3_Column_Int : function (hStmt: PSQLite3Stmt; ColNum: integer): integer; cdecl;
  SQLite3_Column_Text : function (hStmt: PSQLite3Stmt; ColNum: integer): PAnsiChar; cdecl;
  SQLite3_Column_Text16 : function (hStmt: PSQLite3Stmt; ColNum: integer): PChar; cdecl;
  SQLite3_Column_Type : function (hStmt: PSQLite3Stmt; ColNum: integer): integer; cdecl;
  SQLite3_Column_Int64 : function (hStmt: PSQLite3Stmt; ColNum: integer): Int64; cdecl;
  SQLite3_Finalize : function (hStmt: PSQLite3Stmt): integer; cdecl;
  SQLite3_Reset : function (hStmt: PSQLite3Stmt): integer; cdecl;
  SQLite3_Backup_Init : function (DestDb: PSQLite3; DestDbName: PAnsiChar; SourceDb: PSQLite3; SourceDbName: PAnsiChar): PSQLite3Backup; cdecl;
  SQLite3_Backup_Step : function (hBackup: PSQLite3Backup; nPage: integer): integer; cdecl;
  SQLite3_Backup_Finish : function (hBackup: PSQLite3Backup): integer; cdecl;
  SQLite3_Backup_Remaining : function (hBackup: PSQLite3Backup): integer; cdecl;
  SQLite3_Backup_Pagecount : function (hBackup: PSQLite3Backup): integer; cdecl;
  SQLite3_Bind_Blob : function (hStmt: PSQLite3Stmt; ParamNum: integer; ptrData: pointer; numBytes: integer; ptrDestructor: TSQLite3Destructor): integer; cdecl;
  SQLite3_Bind_Text : function (hStmt: PSQLite3Stmt; ParamNum: integer; Text: PAnsiChar; numBytes: integer; ptrDestructor: TSQLite3Destructor): integer; cdecl;
  SQLite3_Bind_Text16 : function (hStmt: PSQLite3Stmt; ParamNum: integer; Text: PChar; numBytes: integer; ptrDestructor: TSQLite3Destructor): integer; cdecl;
  SQLite3_Bind_Double : function (hStmt: PSQLite3Stmt; ParamNum: integer; Data: Double): integer; cdecl;
  SQLite3_Bind_Int : function (hStmt: PSQLite3Stmt; ParamNum: integer; Data: integer): integer; cdecl;
  SQLite3_Bind_Int64 : function (hStmt: PSQLite3Stmt; ParamNum: integer; Data: int64): integer; cdecl;
  SQLite3_Bind_Null : function (hStmt: PSQLite3Stmt; ParamNum: integer): integer; cdecl;
  SQLite3_Bind_Parameter_Index : function (hStmt: PSQLite3Stmt; zName: PAnsiChar): integer; cdecl;
  SQLite3_Enable_Shared_Cache : function (Value: integer): integer; cdecl;
  SQLite3_Create_Collation : function (db: PSQLite3; Name: PAnsiChar; eTextRep: integer; UserData: pointer; xCompare: TCollateXCompare): integer; cdecl;
  SQLite3_Create_Collation16 : function (db: PSQLite3; Name: PChar; eTextRep: integer; UserData: pointer; xCompare: TCollateXCompare): integer; cdecl;

  procedure SQLite3_Dispose_Pointer(ptr: pointer); cdecl;

implementation

uses
{$IFDEF LINUX}
  LibC,
{$ELSE}
  Windows,
{$ENDIF}
  SysUtils;

//Dynamic loading and unloading of Sqlite3 libs
{$IFDEF LINUX}
const
  INVALID_MODULE_HANDLE = nil;

type
  LibHandle = Pointer;

function OpenLib(Name: PChar): LibHandle;
begin
  Result := dlopen(Name);
end;

{$ELSE}
const
  INVALID_MODULE_HANDLE = 0;

type
  LibHandle = HINST;

function OpenLib(Name: PChar): LibHandle;
begin
  Result := LoadLibrary(Name);
end;

{$ENDIF}

var
  Sqlite3Handle: LibHandle = INVALID_MODULE_HANDLE;

procedure ClearSqlite3Proc;
begin
  //sqlite3
  SQLite3_Open := nil;
  SQLite3_Open16 := nil;
  SQLite3_Close := nil;
  SQLite3_Exec := nil;
  SQLite3_Version := nil;
  SQLite3_ErrMsg := nil;
  SQLite3_ErrCode := nil;
  SQLite3_Free := nil;
  SQLite3_GetTable := nil;
  SQLite3_FreeTable := nil;
  SQLite3_Complete := nil;
  SQLite3_LastInsertRowID := nil;
  SQLite3_Interrupt := nil;
  SQLite3_BusyHandler := nil;
  SQLite3_BusyTimeout := nil;
  SQLite3_Changes := nil;
  SQLite3_TotalChanges := nil;
  SQLite3_Prepare := nil;
  SQLite3_Prepare_v2 := nil;
  SQLite3_Prepare16 := nil;
  SQLite3_Prepare16_v2 := nil;
  SQLite3_Column_Count := nil;
  SQLite3_Column_Name := nil;
  SQLite3_Column_Name16 := nil;
  SQLite3_Column_DeclType := nil;
  SQLite3_Column_DeclType16 := nil;
  SQLite3_Step := nil;
  SQLite3_DataCount := nil;
  SQLite3_Column_Blob := nil;
  SQLite3_Column_Bytes := nil;
  SQLite3_Column_Bytes16 := nil;
  SQLite3_Column_Double := nil;
  SQLite3_Column_Int := nil;
  SQLite3_Column_Text := nil;
  SQLite3_Column_Text16 := nil;
  SQLite3_Column_Type := nil;
  SQLite3_Column_Int64 := nil;
  SQLite3_Finalize := nil;
  SQLite3_Reset := nil;
  SQLite3_Backup_Init := nil;
  SQLite3_Backup_Step := nil;
  SQLite3_Backup_Finish := nil;
  SQLite3_Backup_Remaining := nil;
  SQLite3_Backup_Pagecount := nil;
  SQLite3_Bind_Blob := nil;
  SQLite3_Bind_Text := nil;
  SQLite3_Bind_Text16 := nil;
  SQLite3_Bind_Double := nil;
  SQLite3_Bind_Int := nil;
  SQLite3_Bind_Int64 := nil;
  SQLite3_Bind_Null := nil;
  SQLite3_Bind_Parameter_Index := nil;
  SQLite3_Enable_Shared_Cache := nil;
  SQLite3_Create_Collation := nil;
  SQLite3_Create_Collation16 := nil;
end;

procedure LoadSqlite3Proc;
begin
  if Sqlite3Handle <> INVALID_MODULE_HANDLE then
  begin
    //sqlite3
    SQLite3_Open := GetProcAddress(Sqlite3Handle, 'sqlite3_open');
    SQLite3_Open16 := GetProcAddress(Sqlite3Handle, 'sqlite3_open16');
    SQLite3_Close := GetProcAddress(Sqlite3Handle, 'sqlite3_close');
    SQLite3_Exec := GetProcAddress(Sqlite3Handle, 'sqlite3_exec');
    SQLite3_Version := GetProcAddress(Sqlite3Handle, 'sqlite3_libversion');
    SQLite3_ErrMsg := GetProcAddress(Sqlite3Handle, 'sqlite3_errmsg');
    SQLite3_ErrCode := GetProcAddress(Sqlite3Handle, 'sqlite3_errcode');
    SQLite3_Free := GetProcAddress(Sqlite3Handle, 'sqlite3_free');
    SQLite3_GetTable := GetProcAddress(Sqlite3Handle, 'sqlite3_get_table');
    SQLite3_FreeTable := GetProcAddress(Sqlite3Handle, 'sqlite3_free_table');
    SQLite3_Complete := GetProcAddress(Sqlite3Handle, 'sqlite3_complete');
    SQLite3_LastInsertRowID := GetProcAddress(Sqlite3Handle, 'sqlite3_last_insert_rowid');
    SQLite3_Interrupt := GetProcAddress(Sqlite3Handle, 'sqlite3_interrupt');
    SQLite3_BusyHandler := GetProcAddress(Sqlite3Handle, 'sqlite3_busy_handler');
    SQLite3_BusyTimeout := GetProcAddress(Sqlite3Handle, 'sqlite3_busy_timeout');
    SQLite3_Changes := GetProcAddress(Sqlite3Handle, 'sqlite3_changes');
    SQLite3_TotalChanges := GetProcAddress(Sqlite3Handle, 'sqlite3_total_changes');
    SQLite3_Prepare := GetProcAddress(Sqlite3Handle, 'sqlite3_prepare');
    SQLite3_Prepare_v2 := GetProcAddress(Sqlite3Handle, 'sqlite3_prepare_v2');
    SQLite3_Prepare16 := GetProcAddress(Sqlite3Handle, 'sqlite3_prepare16');
    SQLite3_Prepare16_v2 := GetProcAddress(Sqlite3Handle, 'sqlite3_prepare16_v2');
    SQLite3_Column_Count := GetProcAddress(Sqlite3Handle, 'sqlite3_column_count');
    SQLite3_Column_Name := GetProcAddress(Sqlite3Handle, 'sqlite3_column_name');
    SQLite3_Column_Name16 := GetProcAddress(Sqlite3Handle, 'sqlite3_column_name16');
    SQLite3_Column_DeclType := GetProcAddress(Sqlite3Handle, 'sqlite3_column_decltype');
    SQLite3_Column_DeclType16 := GetProcAddress(Sqlite3Handle, 'sqlite3_column_decltype16');
    SQLite3_Step := GetProcAddress(Sqlite3Handle, 'sqlite3_step');
    SQLite3_DataCount := GetProcAddress(Sqlite3Handle, 'sqlite3_data_count');
    SQLite3_Column_Blob := GetProcAddress(Sqlite3Handle, 'sqlite3_column_blob');
    SQLite3_Column_Bytes := GetProcAddress(Sqlite3Handle, 'sqlite3_column_bytes');
    SQLite3_Column_Bytes16 := GetProcAddress(Sqlite3Handle, 'sqlite3_column_bytes16');
    SQLite3_Column_Double := GetProcAddress(Sqlite3Handle, 'sqlite3_column_double');
    SQLite3_Column_Int := GetProcAddress(Sqlite3Handle, 'sqlite3_column_int');
    SQLite3_Column_Text := GetProcAddress(Sqlite3Handle, 'sqlite3_column_text');
    SQLite3_Column_Text16 := GetProcAddress(Sqlite3Handle, 'sqlite3_column_text16');
    SQLite3_Column_Type := GetProcAddress(Sqlite3Handle, 'sqlite3_column_type');
    SQLite3_Column_Int64 := GetProcAddress(Sqlite3Handle, 'sqlite3_column_int64');
    SQLite3_Finalize := GetProcAddress(Sqlite3Handle, 'sqlite3_finalize');
    SQLite3_Reset := GetProcAddress(Sqlite3Handle, 'sqlite3_reset');
    SQLite3_Backup_Init := GetProcAddress(Sqlite3Handle, 'sqlite3_backup_init');
    SQLite3_Backup_Step := GetProcAddress(Sqlite3Handle, 'sqlite3_backup_step');
    SQLite3_Backup_Finish := GetProcAddress(Sqlite3Handle, 'sqlite3_backup_finish');
    SQLite3_Backup_Remaining := GetProcAddress(Sqlite3Handle, 'sqlite3_backup_remaining');
    SQLite3_Backup_Pagecount := GetProcAddress(Sqlite3Handle, 'sqlite3_backup_pagecount');
    SQLite3_Bind_Blob := GetProcAddress(Sqlite3Handle, 'sqlite3_bind_blob');
    SQLite3_Bind_Text := GetProcAddress(Sqlite3Handle, 'sqlite3_bind_text');
    SQLite3_Bind_Text16 := GetProcAddress(Sqlite3Handle, 'sqlite3_bind_text16');
    SQLite3_Bind_Double := GetProcAddress(Sqlite3Handle, 'sqlite3_bind_double');
    SQLite3_Bind_Int := GetProcAddress(Sqlite3Handle, 'sqlite3_bind_int');
    SQLite3_Bind_Int64 := GetProcAddress(Sqlite3Handle, 'sqlite3_bind_int64');
    SQLite3_Bind_Null := GetProcAddress(Sqlite3Handle, 'sqlite3_bind_null');
    SQLite3_Bind_Parameter_Index := GetProcAddress(Sqlite3Handle, 'sqlite3_bind_parameter_index');
    SQLite3_Enable_Shared_Cache := GetProcAddress(Sqlite3Handle, 'sqlite3_enable_shared_cache');
    SQLite3_Create_Collation := GetProcAddress(Sqlite3Handle, 'sqlite3_create_collation');
    SQLite3_Create_Collation16 := GetProcAddress(Sqlite3Handle, 'sqlite3_create_collation');
  end;
end;

function loadSqlite3: boolean;
begin
  if Sqlite3Handle = INVALID_MODULE_HANDLE then
    Result := LoadSqlite3From(SQLITE3_NAME)
  else
    Result := True;
end;

function loadSqlite3From(FileName: string): boolean;
begin
  ClearSqlite3Proc;
  Sqlite3Handle := OpenLib(PChar(FileName));
  if Sqlite3Handle <> INVALID_MODULE_HANDLE then
  begin
    LoadSqlite3Proc;
    Result := True;
  end
  else
    Result := False;
end;

procedure unLoadSqlite3;
begin
  if Sqlite3Handle <> INVALID_MODULE_HANDLE then
    FreeLibrary(Sqlite3Handle);
  Sqlite3Handle := INVALID_MODULE_HANDLE;
  ClearSqlite3Proc;
end;

function isSqlite3Loaded: boolean;
begin
  Result := Sqlite3Handle <> INVALID_MODULE_HANDLE;
end;

procedure SQLite3_Dispose_Pointer(ptr: pointer); cdecl;
begin
  if assigned(ptr) then
    freemem(ptr);
end;

end.

