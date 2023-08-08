unit amLocalization.ResourceWriter;

(*
 * Copyright © 2019 Anders Melander
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *)

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Classes,
  Windows,
  amLocalization.Model;

// -----------------------------------------------------------------------------
//
// TResourceWriter
//
// -----------------------------------------------------------------------------
type
  IResourceWriter = interface
    procedure BeginWrite;
    procedure EndWrite(Commit: boolean);

    procedure WriteModule(Module: TLocalizerModule; ResourceID: PWideChar; Stream: TMemoryStream);
    procedure CopyVersionInfo(Stream: TMemoryStream);
  end;


// -----------------------------------------------------------------------------
//
// TResourceFileWriter
//
// -----------------------------------------------------------------------------
// Write localized data as individual DFM files.
// -----------------------------------------------------------------------------
type
  TResourceFileWriter = class(TInterfacedObject, IResourceWriter)
  public
    procedure BeginWrite;
    procedure EndWrite(Commit: boolean);
    procedure WriteModule(Module: TLocalizerModule; ResourceID: PWideChar; Stream: TMemoryStream);
    procedure CopyVersionInfo(Stream: TMemoryStream);
  end;


// -----------------------------------------------------------------------------
//
// TResourceModuleWriter
//
// -----------------------------------------------------------------------------
// Update resource module with localized data.
// -----------------------------------------------------------------------------
  TResourceModuleWriter = class(TInterfacedObject, IResourceWriter)
  private
    FFilename: string;
    FResourceHandle: THandle;
  protected
    procedure WriteFormModule(Module: TLocalizerModule; ResourceID: PWideChar; Stream: TMemoryStream);
    procedure WriteStringModule(Module: TLocalizerModule; ResourceID: PWideChar; Stream: TMemoryStream);
  public
    constructor Create(const AFilename: string);

    procedure BeginWrite;
    procedure EndWrite(Commit: boolean);
    procedure WriteModule(Module: TLocalizerModule; ResourceID: PWideChar; Stream: TMemoryStream);
    procedure CopyVersionInfo(Stream: TMemoryStream);
  end;

var
  sResourceModuleStub: string = 'EmptyResourceModule.dll';


// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

implementation

uses
  SysUtils,
  IOUtils,
  Dialogs,
  System.UITypes,
  Controls;

const
  sResourceModuleStubRes = 'EmptyResourceModule'; // Name of above file as an embedded resource

  // The following is the content of the file specified by the sResourceModuleStub constant.
  // It should be a minimal (i.e. no code) PE module containing nothing but a resource section without any resources.
  sResourceModuleData : AnsiString =
    #$4D#$5A#$90#$00#$03#$00#$00#$00#$04#$00#$00#$00#$FF#$FF#$00#$00#$B8#$00#$00#$00#$00#$00#$00#$00#$40#$00#$00#$00#$00#$00#$00#$00+
    #$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$B0#$00#$00#$00+
    #$0E#$1F#$BA#$0E#$00#$B4#$09#$CD#$21#$B8#$01#$4C#$CD#$21#$54#$68#$69#$73#$20#$70#$72#$6F#$67#$72#$61#$6D#$20#$63#$61#$6E#$6E#$6F+
    #$74#$20#$62#$65#$20#$72#$75#$6E#$20#$69#$6E#$20#$44#$4F#$53#$20#$6D#$6F#$64#$65#$2E#$0D#$0D#$0A#$24#$00#$00#$00#$00#$00#$00#$00+
    #$37#$CF#$3C#$DF#$73#$AE#$52#$8C#$73#$AE#$52#$8C#$73#$AE#$52#$8C#$E1#$F0#$AD#$8C#$72#$AE#$52#$8C#$E4#$F0#$50#$8D#$72#$AE#$52#$8C+
    #$52#$69#$63#$68#$73#$AE#$52#$8C#$00#$00#$00#$00#$00#$00#$00#$00#$50#$45#$00#$00#$4C#$01#$02#$00#$B0#$66#$81#$5A#$00#$00#$00#$00+
    #$00#$00#$00#$00#$E0#$00#$02#$21#$0B#$01#$0E#$00#$00#$00#$00#$00#$00#$04#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$10#$00#$00+
    #$00#$10#$00#$00#$00#$00#$00#$10#$00#$10#$00#$00#$00#$02#$00#$00#$06#$00#$00#$00#$00#$00#$00#$00#$06#$00#$00#$00#$00#$00#$00#$00+
    #$00#$30#$00#$00#$00#$02#$00#$00#$00#$00#$00#$00#$02#$00#$40#$05#$00#$00#$10#$00#$00#$10#$00#$00#$00#$00#$10#$00#$00#$10#$00#$00+
    #$00#$00#$00#$00#$10#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$20#$00#$00#$10#$00#$00#$00+
    #$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$10#$00#$00#$1C#$00#$00#$00+
    #$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00+
    #$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00+
    #$00#$00#$00#$00#$00#$00#$00#$00#$2E#$72#$64#$61#$74#$61#$00#$00#$70#$00#$00#$00#$00#$10#$00#$00#$00#$02#$00#$00#$00#$02#$00#$00+
    #$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$40#$00#$00#$40#$2E#$72#$73#$72#$63#$00#$00#$00#$10#$00#$00#$00#$00#$20#$00#$00+
    #$00#$02#$00#$00#$00#$04#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$40#$00#$00#$40#$00#$00#$00#$00#$00#$00#$00#$00+
    #$00#$00#$00#$00#$B0#$66#$81#$5A#$00#$00#$00#$00#$0D#$00#$00#$00#$40#$00#$00#$00#$1C#$10#$00#$00#$1C#$02#$00#$00#$00#$00#$00#$00+
    #$00#$10#$00#$00#$1C#$00#$00#$00#$2E#$72#$64#$61#$74#$61#$00#$00#$1C#$10#$00#$00#$54#$00#$00#$00#$2E#$72#$64#$61#$74#$61#$24#$7A+
    #$7A#$7A#$64#$62#$67#$00#$00#$00#$00#$20#$00#$00#$10#$00#$00#$00#$2E#$72#$73#$72#$63#$24#$30#$31#$00#$00#$00#$00#$00#$00#$00#$00+
    #$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00+
    #$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00+
    #$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00+
    #$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00+
    #$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00+
    #$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00+
    #$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00+
    #$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00+
    #$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00+
    #$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00+
    #$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00+
    #$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00+
    #$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00+
    #$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00+
    #$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00+
    #$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00+
    #$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00+
    #$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00+
    #$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00+
    #$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00+
    #$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00+
    #$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00+
    #$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00+
    #$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00+
    #$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00+
    #$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00+
    #$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00+
    #$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00+
    #$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00;


// -----------------------------------------------------------------------------
//
// TResourceFileWriter
//
// -----------------------------------------------------------------------------
procedure TResourceFileWriter.BeginWrite;
begin
end;

procedure TResourceFileWriter.CopyVersionInfo(Stream: TMemoryStream);
begin
end;

procedure TResourceFileWriter.EndWrite(Commit: boolean);
begin
end;

procedure TResourceFileWriter.WriteModule(Module: TLocalizerModule; ResourceID: PWideChar; Stream: TMemoryStream);
var
  Filename: string;
  DFMStream: TStream;
begin
  // TODO : Handle other module types
  if (Module.Kind <> mkForm) then
    exit;

  Filename := Format('localized\%s.dfm', [Module.Name]);

  TDirectory.CreateDirectory(TPath.GetDirectoryName(Filename));

  DFMStream := TFileStream.Create(Filename, fmCreate);
  try

    ObjectBinaryToText(Stream, DFMStream);

  finally
    DFMStream.Free;
  end;
end;


// -----------------------------------------------------------------------------
//
// TResourceModuleWriter
//
// -----------------------------------------------------------------------------
constructor TResourceModuleWriter.Create(const AFilename: string);
begin
  inherited Create;
  FFilename := AFilename;
end;

// -----------------------------------------------------------------------------

procedure TResourceModuleWriter.CopyVersionInfo(Stream: TMemoryStream);
begin
  Win32Check(UpdateResource(FResourceHandle, RT_VERSION, PWideChar(1), MAKELANGID(LANG_NEUTRAL, SUBLANG_NEUTRAL), Stream.Memory, Stream.Size));
end;

// -----------------------------------------------------------------------------

procedure TResourceModuleWriter.BeginWrite;
var
  SourceStream, TargetStream: TStream;
  Filename: string;
begin
  // External DLL stub is located in same folder as application
  Filename := TPath.Combine(TPath.GetDirectoryName(ParamStr(0)), sResourceModuleStub);

  // Use external DLL stub if it exist
  if (TFile.Exists(Filename)) then
  begin

    TFile.Copy(Filename, FFilename, True);

  end else
  if (FindResource(hInstance, PChar(sResourceModuleStubRes), RT_RCDATA) <> 0) then
  begin
    // Otherwise use embedded resource

    SourceStream := TResourceStream.Create(hInstance, PChar(sResourceModuleStubRes), RT_RCDATA);
    try

      TargetStream := TFileStream.Create(FFilename, fmCreate);
      try

        TargetStream.CopyFrom(SourceStream, 0);

      finally
        TargetStream.Free;
      end;

    finally
      SourceStream.Free;
    end;

  end else
  begin

    // Otherwise use internal stub (by default they should all three be the same)

    TargetStream := TFileStream.Create(FFilename, fmCreate);
    try

      TargetStream.WriteBuffer(PAnsiChar(sResourceModuleData)^, Length(sResourceModuleData));

    finally
      TargetStream.Free;
    end;

  end;

  FResourceHandle := BeginUpdateResource(PChar(FFilename), True);

  if (FResourceHandle = 0) then
    RaiseLastOSError;
end;

// -----------------------------------------------------------------------------

procedure TResourceModuleWriter.EndWrite(Commit: boolean);
resourcestring
  sModuleWriteAccessDeniedTitle = 'Error writing resource module';
  sModuleWriteAccessDeniedMsg = 'The resource module could not be updated.'#13+
    'Filename: %s'#13+
    'Error: %s'#13#13+
    'Please make sure that your anti virus program isn''t preventing the file from being updated';
begin
  try

    while (True) do
    begin
      try

        if (FResourceHandle <> 0) then
          Win32Check(EndUpdateResourceW(FResourceHandle, not Commit));

        break;

      except
        on E: EOSError do
        begin
          if (Commit) and (E.ErrorCode = ERROR_ACCESS_DENIED) then
          begin
            var Res := TaskMessageDlg(sModuleWriteAccessDeniedTitle, Format(sModuleWriteAccessDeniedMsg, [FFilename, E.Message]),
              mtWarning, [mbAbort, mbRetry], 0, mbRetry);

            if (Res <> mrRetry) then
              Abort;
          end else
          // Retry after anti-virus has deleted the temp file created by BeginUpdateResource has been
          // observed to cause internal AVs returned as ERROR_INTERNAL_ERROR or ERROR_INVALID_HANDLE.
          if (E.ErrorCode = ERROR_INTERNAL_ERROR) or (E.ErrorCode = ERROR_INVALID_HANDLE) then
          begin
            TaskMessageDlg(sModuleWriteAccessDeniedTitle, Format(sModuleWriteAccessDeniedMsg, [FFilename, E.Message]),
              mtError, [mbAbort], 0);
            FResourceHandle := 0;
            Abort;
          end else
            raise;
        end;
      end;

    end;

  except
    if (FResourceHandle <> 0) and (Commit) then
      EndUpdateResourceW(FResourceHandle, True);
    // TODO : Should we delete the file here?
    raise;
  end;
end;

// -----------------------------------------------------------------------------

procedure TResourceModuleWriter.WriteFormModule(Module: TLocalizerModule; ResourceID: PWideChar; Stream: TMemoryStream);
begin
  Win32Check(UpdateResource(FResourceHandle, RT_RCDATA, ResourceID, MAKELANGID(LANG_NEUTRAL, SUBLANG_NEUTRAL), Stream.Memory, Stream.Size));
end;

procedure TResourceModuleWriter.WriteStringModule(Module: TLocalizerModule; ResourceID: PWideChar; Stream: TMemoryStream);
begin
  Win32Check(UpdateResource(FResourceHandle, RT_STRING, ResourceID, MAKELANGID(LANG_NEUTRAL, SUBLANG_NEUTRAL), Stream.Memory, Stream.Size));
end;

procedure TResourceModuleWriter.WriteModule(Module: TLocalizerModule; ResourceID: PWideChar; Stream: TMemoryStream);
begin
  if (Module.Kind = mkForm) then
    WriteFormModule(Module, ResourceID, Stream)
  else
  if (Module.Kind = mkString) then
    WriteStringModule(Module, ResourceID, Stream);
end;

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

end.
