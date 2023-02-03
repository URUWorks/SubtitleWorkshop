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
  UWSystem.FontUtils, UWTranslateAPI.Microsoft, 
  UWSubtitleAPI.Formats.Captions32, UWSubtitleAPI.Formats.CaptionsInc, 
  UWSubtitleAPI.Formats.Cavena890, UWSubtitleAPI.Formats.Cheetah, 
  UWSubtitleAPI.Formats.CheetahCaption, 
  UWSubtitleAPI.Formats.CheetahCaption.Types, UWSubtitleAPI.Formats.CPC600, 
  UWSubtitleAPI.Formats.DKS, UWSubtitleAPI.Formats.DRTIC, 
  UWSubtitleAPI.Formats.DVDJunior, UWSubtitleAPI.Formats.DVDSubtitle, 
  UWSubtitleAPI.Formats.DVDSubtitleSystem, UWSubtitleAPI.Formats.EBU, 
  UWSubtitleAPI.Formats.FABSubtitler, UWSubtitleAPI.Formats.GPACTTXT, 
  UWSubtitleAPI.Formats.IAuthor, UWSubtitleAPI.Formats.InscriberCG, 
  UWSubtitleAPI.Formats.ITunesTimedText, UWSubtitleAPI.Formats.JACOSub, 
  UWSubtitleAPI.Formats.KaraokeLyricsLRC, 
  UWSubtitleAPI.Formats.KaraokeLyricsVKT, 
  UWSubtitleAPI.Formats.MacDVDStudioPro, UWSubtitleAPI.Formats.MacSUB, 
  UWSubtitleAPI.Formats.MicroDVD, UWSubtitleAPI.Formats.MPlayer, 
  UWSubtitleAPI.Formats.MPlayer2, UWSubtitleAPI.Formats.NetflixTimedText, 
  UWSubtitleAPI.Formats.SBV, UWSubtitleAPI.Formats.Sofni, 
  UWSubtitleAPI.Formats.STL, UWSubtitleAPI.Formats.WebVTT, LazarusPackageIntf;

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
