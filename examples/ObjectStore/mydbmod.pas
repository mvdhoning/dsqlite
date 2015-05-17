{$mode objfpc}{$h+}{$M+}
unit mydbmod;

interface

uses classes, model;

type

  TMyData = class(TModel)
  private
    fid: integer;
    fname: String;
  public
  published
    property Id: integer read fid write fid;
    property Name: String read fname write fname;
  end;

implementation

initialization
RegisterClass(TMyData);

end.