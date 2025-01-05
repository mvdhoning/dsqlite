{$mode objfpc}{$h+}{$M+} 
unit model;

//This Source Code Form is subject to the terms of the Mozilla Public
//License, v. 2.0. If a copy of the MPL was not distributed with this
//file, You can obtain one at http://mozilla.org/MPL/2.0/.

interface

uses sqlite3, classes, sysutils;

type
  
  TModel = class(TComponent)
  private
    ftablename: string;
    fguid: string;
  public
    property TableName: String read ftablename write ftablename;
    property Guid: string read fguid write fguid;
  end;

implementation

end.
