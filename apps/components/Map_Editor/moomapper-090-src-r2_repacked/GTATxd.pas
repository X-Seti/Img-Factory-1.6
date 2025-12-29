unit GTATxd;

interface

uses OpenGl, Windows, Classes, SysUtils, Dialogs;

const
  GL_RGBA4 = $8056;
  GL_RGB4 = $804F;
  GL_COMPRESSED_RGBA_ARB = $84EE;
  GL_COLOR_INDEX8_EXT = $80E5;
  GL_COMPRESSED_RGB_S3TC_DXT1_EXT = $83F0;
  GL_COMPRESSED_RGBA_S3TC_DXT1_EXT = $83F1;
  GL_COMPRESSED_RGBA_S3TC_DXT3_EXT = $83F2;
  GL_COMPRESSED_RGBA_S3TC_DXT5_EXT = $83F3;

type
  TTexture = record
    Name, AlphaName: String;

    Width, Height, Alpha: Word;
    MipMaps, Other, Compression: Byte;
    Depth: Byte;

    DataSize: LongWord;

    Data: PChar;
    Palette: PChar;
  end;

  TGTATxd = class
  private
    FData: PChar;

    Stream: TStream;
    FStart, FSize: Int64;

    procedure ParseFile;
  public
    Name: String;
    Loaded, InUse: Boolean;

    Image: array of TTexture;
    ImageTexture: array of GLuint;
    ImageCount: LongWord;
    ImageNameList, ImageAlphaList: TStringList;

    constructor Create; overload;
    constructor Create(st: TStream; in_name: String; in_start, in_size: Int64); overload;
    destructor Destroy; override;

    procedure LoadFromStream;
    procedure Unload;
  end;

implementation

uses GLView, Main;

constructor TGTATxd.Create;
begin
  inherited Create;
end;

constructor TGTATxd.Create(st: TStream; in_name: String; in_start, in_size: Int64);
begin
  inherited Create;
  ImageNameList := TStringList.Create;
  ImageNameList.CaseSensitive := False;
  ImageAlphaList := TStringList.Create;
  ImageAlphaList.CaseSensitive := False;
  Name := ChangeFileExt(in_name, '');
  Loaded := False;
  Stream := St;
  FStart := in_start;
  FSize := in_size;
  InUse := False;
  if not GTA_TEXTURE_WHEN_NEEDED and not GTA_TEXTURE_LOAD_DEMAND then
    LoadFromStream;
end;

destructor TGTATxd.Destroy;
begin
  inherited Destroy;
  if Loaded then
    glDeleteTextures(ImageCount, ImageTexture[0]);
  ImageNameList.Free;
  ImageAlphaList.Free;
end;

procedure TGTATxd.LoadFromStream;
begin
  Loaded := True;
  GetMem(FData, FSize);

  Stream.Seek(FStart, soFromBeginning);
  Stream.ReadBuffer(FData^, FSize);

  ParseFile;

  FreeMem(FData, FSize);
end;

procedure TGTATxd.Unload;
begin
  if Loaded then
    glDeleteTextures(ImageCount, ImageTexture[0]);
  Loaded := False;
  ImageCount := 0;
  SetLength(Image, ImageCount);
  SetLength(ImageTexture, ImageCount);
end;

procedure TGTATxd.ParseFile;
var
  I, J, DataType: LongWord;
  FFilePos: Int64;
  W, H: Word;
begin
  if (FData = nil) then
    Exit;

  FFilePos := 24;

  DataType := PInteger(FData + FFilePos + 4)^;
  if (DataType = 21) then
    ImageCount := PInteger(FData + FFilePos)^
  else
    ImageCount := 0;
  SetLength(Image, ImageCount);
  SetLength(ImageTexture, ImageCount);
  ImageNameList.Clear;
  ImageAlphaList.Clear;

  FFilePos := FFilePos + 4;

  if (ImageCount > 0) then
  begin

    if (PInteger(FData + FFilePos + 24)^ = 3298128) then
    begin
      ImageCount := 0;
      SetLength(Image, ImageCount);
      SetLength(ImageTexture, ImageCount);
    end else
    begin
      glGenTextures(ImageCount, ImageTexture[0]);

      for I := 0 to ImageCount - 1 do with Image[I] do
      begin
        FFilePos := FFilePos + 12;

        FFilePos := FFilePos + 20;

        Image[I].Name := Trim(PChar(FData + FFilePos));
        FFilePos := FFilePos + 32;
        ImageNameList.AddObject(Trim(Image[I].Name), Pointer(I));

        AlphaName := Trim(PChar(FData + FFilePos));
        FFilePos := FFilePos + 32 + 4;
        ImageAlphaList.AddObject(Trim(AlphaName), Pointer(I));

        Alpha := Word(PInteger(FData + FFilePos)^);
        Width := Word(PInteger(FData + FFilePos + 4)^);
        Height := Word(PInteger(FData + FFilePos + 6)^);
        Depth := Byte(PInteger(FData + FFilePos + 8)^);
        MipMaps := Byte(PInteger(FData + FFilePos + 9)^);
        Other := Byte(PInteger(FData + FFilePos + 10)^);
        Compression := Byte(PInteger(FData + FFilePos + 11)^);
        FFilePos := FFilePos + 12;

        glBindTexture(GL_TEXTURE_2D, ImageTexture[I]);

        // skip over other mipmaps
        if (MipMaps > 0) then for J := 0 to MipMaps - 1 do
        begin
          if (Depth = 8) then
          begin
            Palette := FData + FFilePos;
            FFilePos := FFilePos + 1024;
          end;

          DataSize := PInteger(FData + FFilePos)^;
          FFilePos := FFilePos + 4;

          Data := FData + FFilePos;
          FFilePos := FFilePos + DataSize;

          W := Width div (1 shl J);
          H := Height div (1 shl J);
          if (W = 0) then W := 1;
          if (H = 0) then H := 1;

          case Depth of
            8:
            begin
              if Assigned(glColorTableEXT) then
              begin
                glTexImage2D(GL_TEXTURE_2D, J, GL_COLOR_INDEX8_EXT, W, H, 0, GL_COLOR_INDEX, GL_UNSIGNED_BYTE, Data);
                glColorTableEXT(GL_TEXTURE_2D, GL_RGBA4, 256, GL_RGBA, GL_UNSIGNED_BYTE, Palette^);
              end;
            end;

            16:
            begin
              if Assigned(glCompressedTexImage2DARB) then
              begin
                case Compression of
                  1: if (Alpha = 0) then
                      glCompressedTexImage2DARB(GL_TEXTURE_2D, J, GL_COMPRESSED_RGB_S3TC_DXT1_EXT, W, H, 0, DataSize, Data)
                    else
                      glCompressedTexImage2DARB(GL_TEXTURE_2D, J, GL_COMPRESSED_RGBA_S3TC_DXT1_EXT, W, H, 0, DataSize, Data);
                  3: glCompressedTexImage2DARB(GL_TEXTURE_2D, J, GL_COMPRESSED_RGBA_S3TC_DXT3_EXT, W, H, 0, DataSize, Data);
                  5: glCompressedTexImage2DARB(GL_TEXTURE_2D, J, GL_COMPRESSED_RGBA_S3TC_DXT5_EXT, W, H, 0, DataSize, Data);
                end;
              end;
            end;

            32:
            begin
              if (Alpha = 0) then
                glTexImage2D(GL_TEXTURE_2D, J, GL_RGBA4, W, H, 0, GL_RGBA, GL_UNSIGNED_BYTE, Data);
              begin
                glTexImage2D(GL_TEXTURE_2D, J, GL_RGBA4, W, H, 0, GL_RGBA, GL_UNSIGNED_BYTE, Data);
              end;
            end;
          end;
        end;

        // set texture paremeters
        glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

        if (MipMaps > 1) then
        	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_NEAREST)
        else
        	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);

        if (I < ImageCount - 1) and not (PInteger(FData + FFilePos)^ = 3) then
          if (PInteger(FData + FFilePos + 2)^ = 3) then
            FFilePos := FFilePos + 2
          else if (PInteger(FData + FFilePos - 2)^ = 3) then
            FFilePos := FFilePos - 2;

        FFilePos := FFilePos + 12;
      end;
    end;

  end;

  ImageNameList.Sort;
  ImageAlphaList.Sort;
end;

end.
