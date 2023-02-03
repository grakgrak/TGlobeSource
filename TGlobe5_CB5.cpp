//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("TGlobe5_CB5.res");
USEPACKAGE("vcl50.bpi");
USEUNIT("..\Source\GBitmap.pas");
USEUNIT("..\Source\GClasses.pas");
USEUNIT("..\Source\GConversions.pas");
USEUNIT("..\Source\GCSVReader.pas");
USEUNIT("..\Source\GDataLink.pas");
USEUNIT("..\Source\GDataReaders.pas");
USEUNIT("..\Source\GDataWriters.pas");
USEUNIT("..\Source\GDBFReader.pas");
USEUNIT("..\Source\GE00Reader.pas");
USEUNIT("..\Source\GGraticule.pas");
USEUNIT("..\Source\GLabelRenderer.pas");
USEUNIT("..\Source\Globe5.pas");
USEUNIT("..\Source\Globe5Utils.pas");
USEUNIT("..\Source\GLogging.pas");
USEUNIT("..\Source\GLR5Reader.pas");
USEUNIT("..\Source\GLR5Writer.pas");
USEUNIT("..\Source\GLYRReader.pas");
USEUNIT("..\Source\GLYRWriter.pas");
USEUNIT("..\Source\GMapObjectEditors.pas");
USEUNIT("..\Source\GMapObjects.pas");
USEUNIT("..\Source\GMemObjectSource.pas");
USEUNIT("..\Source\GMIFReader.pas");
USEUNIT("..\Source\GMorphing.pas");
USEUNIT("..\Source\GPresenters.pas");
USEUNIT("..\Source\GProjections.pas");
USEUNIT("..\Source\GRenderer.pas");
USEUNIT("..\Source\GResource.pas");
USEUNIT("..\Source\GRoadPresenter.pas");
USEUNIT("..\Source\GSDBCache.pas");
USEUNIT("..\Source\GSDBBTree.pas");
USEUNIT("..\Source\GSDBCrypt.pas");
USEUNIT("..\Source\GSDBDataLink.pas");
USEUNIT("..\Source\GSDBIndex.pas");
USEUNIT("..\Source\GSDBKeyIndex.pas");
USEUNIT("..\Source\GSDBLayers.pas");
USEUNIT("..\Source\GSDBObjectSource.pas");
USEUNIT("..\Source\GSDBRTree.pas");
USEUNIT("..\Source\GSDBTables.pas");
USEUNIT("..\Source\GSHPReader.pas");
USEUNIT("..\Source\GSmoothPolyPresenter.pas");
USEUNIT("..\Source\GSpatialDatabase.pas");
USEUNIT("..\Source\GSysUtils.pas");
USEUNIT("..\Source\GTextReader.pas");
USEUNIT("..\Source\GWindBarbPresenter.pas");
USEUNIT("..\Source\GXML.pas");
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------

//   Package source.
//---------------------------------------------------------------------------

#pragma argsused
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
        return 1;
}
//---------------------------------------------------------------------------
