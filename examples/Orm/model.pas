{$mode objfpc}{$h+}{$M+} 
unit model;

//This Source Code Form is subject to the terms of the Mozilla Public
//License, v. 2.0. If a copy of the MPL was not distributed with this
//file, You can obtain one at http://mozilla.org/MPL/2.0/.

interface

uses sqlite3, classes, sysutils;

type
  
  TModel = class(TPersistent)
  private
    fguid: string;
  public
    property Guid: string read fguid write fguid;
  end;

  TModelType = class of TModel;

implementation

end.
