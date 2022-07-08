{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit UWLazarus;

{$warn 5023 off : no warning about unused units}
interface

uses
  UWControls, UWControls.WaveformDisplay, UWControls.Utils, UWFiles.MRU, 
  UWParser.Wave, UWSpellCheck.Hunspell, UWSystem.Encoding, 
  UWSystem.Globalization, UWSystem.Graphics, UWSystem.HTMLUtils, 
  UWSystem.StrUtils, UWSystem.SysUtils, UWSystem.TimeUtils, UWSystem.XmlUtils, 
  UWSubtitles.OCR, UWSubtitles.Utils, UWSystem.InetUtils, 
  UWTranslateAPI.Google, UWSubtitleAPI.ExtraInfo, 
  UWSubtitleAPI.Formats.ABCiView, UWSubtitleAPI.Formats.AdobeEncoreDVD, 
  UWSubtitleAPI.Formats.AdvancedSubstationAlpha, 
  UWSubtitleAPI.Formats.AdvancedSubtitles, UWSubtitleAPI.Formats.AQTitle, 
  UWSubtitleAPI.Formats.AvidCaption, UWSubtitleAPI.Formats.Cavena890.Types, 
  UWSubtitleAPI.Formats.EBU.Types, UWSubtitleAPI.Formats, 
  UWSubtitleAPI.Formats.SubRip, UWSubtitleAPI.Formats.TimedText, 
  UWSubtitleAPI, UWSubtitleAPI.Tags, UWSubtitleAPI.TimeCode, 
  UWSystem.FontUtils, UWTranslateAPI.Microsoft, UWImport.libmpv, 
  UWMediaEngine.Thread, UWMediaEngine, UWMediaEngine.libMPV, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('UWControls', @UWControls.Register);
  RegisterUnit('UWControls.WaveformDisplay', 
    @UWControls.WaveformDisplay.Register);
end;

initialization
  RegisterPackage('UWLazarus', @Register);
end.
