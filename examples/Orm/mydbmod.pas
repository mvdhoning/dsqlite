{$mode objfpc}{$h+}{$M+}
unit mydbmod;

// Any copyright is dedicated to the Public Domain.
// http://creativecommons.org/publicdomain/zero/1.0/

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

  TAuthor = class(TModel)
  private
    fname: String;
  public
  published
    property Name: String read fname write fname;
  end;

  TNote = class(TModel)
    private
      ftitle: String;
      ftext: String;
      fauthor: TAuthor;
    public
      Destructor Destroy(); override;
    published
      property Title: String read ftitle write ftitle;
      property Text: String read ftext write ftext;
      property Author: TAuthor read fauthor write fauthor;
    end;

implementation

uses SysUtils;

Destructor TNote.Destroy();
begin
  FreeAndNil(fauthor);
  inherited;
end;



initialization
RegisterClass(TMyData);
RegisterClass(TAuthor);
RegisterClass(TNote);

end.
