//-------------------------------------------------------------------------------
//Summary
//TGlobe Defines Include File.
//
//Description
//Identifies the compiler version and controls some of the component compilation.
//
//Author
//Graham Knight (tglobe@tglobe.com)
//-------------------------------------------------------------------------------

{$IFDEF VER150}
  {$IFDEF BCB}
    {$DEFINE TG_BCB7}
  {$ELSE}
    {$DEFINE TG_DELPHI7}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER140}
  {$IFDEF BCB}
    {$DEFINE TG_BCB6}
  {$ELSE}
    {$DEFINE TG_DELPHI6}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER130}
  {$IFDEF BCB}
    {$DEFINE TG_BCB5}
  {$ELSE}
    {$DEFINE TG_DELPHI5}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER125}
  {$DEFINE TG_BCB4}
  {$DEFINE BCB}
{$ENDIF}

{$IFDEF VER120}
  {$DEFINE TG_DELPHI4}
{$ENDIF}

{$IFDEF VER110}
  {$DEFINE TG_BCB3}
{$ENDIF}

{$IFDEF VER100}
  {$DEFINE TG_DELPHI3}
{$ENDIF}

{$IFDEF BCB}
  {$DEFINE TG_CBUILDER}
{$ENDIF}


{$IFDEF TG_DELPHI5}
  {$DEFINE TG_DELPHI5_UP}
{$ENDIF}

{$IFDEF TG_DELPHI6}
  {$DEFINE TG_DELPHI5_UP}
  {$DEFINE TG_DELPHI6_UP}
{$ENDIF}

{$IFDEF TG_DELPHI7}
  {$DEFINE TG_DELPHI5_UP}
  {$DEFINE TG_DELPHI6_UP}
  {$DEFINE TG_DELPHI7_UP}
{$ENDIF}


{$IFDEF TG_DELPHI7}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_TYPE OFF}
{$ENDIF}
