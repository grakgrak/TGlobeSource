{-------------------------------------------------------------------------
 Module:    Spatial Database Rtree classes

 Comment:

 Classes:   TGRTreeIndex
            TGRTreeNode

 Author:    Graham Knight
 Email:     tglobe@tglobe.com
-------------------------------------------------------------------------}
{$I GLOBE5.INC}
{$RANGECHECKS OFF}

unit GSDBRtree;

interface

Uses  Classes, Windows, SysUtils, GSDBCache, GSysUtils, GSDBTables, GSDBIndex,
  GXML;

const
  RTREE_MAGIC = $52540000;
  RTREE_MAGIC1 = RTREE_MAGIC + 1;

type
  // Header record for each page in the index
  TGRTreePageRec = packed record
    ParentID : TGPageID;
    LeafNode : boolean;
    NodeCount : Word;
  end;

  // individual tuple records in the index
  TGRTreeTupleRec = packed record
    MER : TGMER;
    RefID : TGPrimaryKey;
  end;
  PTGRTreeTupleRec = ^TGRTreeTupleRec;
  TGRTreeTupleArray = array of TGRTreeTupleRec;

//const
//  RTREE_MAX_ENTRIES = ( CACHE_PAGE_SIZE - Sizeof(TGRTreePageRec)) div SizeOf(TGRTreeTupleRec);
//  RTREE_MIN_ENTRIES = RTREE_MAX_ENTRIES div 4;
var
  RTREE_MAX_ENTRIES : integer = 0;
  RTREE_MIN_ENTRIES : integer = 0;

type
  TGRTreeIndex = class;
  TGRTreeNode = class;

  // record containing the data in a page
  TGRTreeNodeRec = packed record
    Header : TGRTreePageRec;
    Tuples : array [0..0] of TGRTreeTupleRec;
  end;
  PTGRTreeNodeRec = ^TGRTreeNodeRec;

  // Wrapper class to manage access to the RTree node
  TGRTreeNode = class( TGPage )
  private
    FNodePtr : PTGRTreeNodeRec;

    function GetCount: integer;
    function GetIsLeaf: boolean;
    function GetParentID: TGPageID;
    function GetTuple(index: integer): PTGRTreeTupleRec;
    procedure SetIsLeaf(const Value: boolean);
    procedure SetParentID(const Value: TGPageID);
    procedure SetCount(const Value: integer);
    procedure SetTuple(index: integer; const Value: PTGRTreeTupleRec);
  public
    constructor Create( pageCache: TGPageCache; pageID : TGPageID ); override;

    procedure AddTuple( mer : TGMER; refID : TGPrimaryKey );
    procedure DeleteTuple( refID : TGPrimaryKey );
    procedure UpdateTuple( idx : integer; mer : TGMER; refID : TGPrimaryKey );

    function NodeMER : TGMER;
    function IndexOf( refID : TGPrimaryKey ) : integer;

    property Count : integer read GetCount write SetCount;
    property ParentID : TGPageID read GetParentID write SetParentID;
    property Tuple[index : integer] : PTGRTreeTupleRec read GetTuple write SetTuple; default;
    property IsLeaf : boolean read GetIsLeaf write SetIsLeaf;
  end;

  // Header record for an RTree index
  TGRTreeHeader = packed record
    Magic : DWORD;
    RootNodeID : TGPageID;
    PageCount : integer;
    TreeDepth : integer;
    IndexMER : TGMER;
  end;

  //---------------------------------------------------------------------------
  //Description
  //\Internal Spatial Database class that provides an RTree index used for fast
  //access using MER's to control object selection.
  //---------------------------------------------------------------------------
  TGRTreeIndex = class( TGSpatialIndex )
  private
    FHeader: TGRTreeHeader;

    FNodeCount : Cardinal;
    FIndexMERValid : Boolean;
    FIndexMER : TGMER;

    FResultList : TList;
    FAbortSearch : Boolean;

    function ChooseLeaf( nodeID: TGPageID; mer : TGMER ) : TGPageID;
    procedure AdjustTree( nodeID, splitID: TGPageID );
    function SplitNode( nodeID: TGPageID; const mer : TGMER; refID : TGPrimaryKey ) : TGPageID;
    function NewNode : TGPageID;
    function FindLeaf( nodeID: TGPageID; const mer : TGMER; refID : TGPrimaryKey) : TGPageID;
    procedure CondenseTree( nodeID: TGPageID; var list : TGPageIDArray );
    procedure ReInsertNode( nodeID : TGPageID );
    procedure GrowTree( splitID : TGPageID );
    procedure ShrinkTree;

    procedure PickSeeds( var list : TGRTreeTupleArray; node, split : TGRTreeNode );
    procedure SearchNode( nodeID: TGPageID; minSize : integer; const searchMER : TGMER; callback : TGSearchCallback );
    procedure SearchTree( searchMER : TGMER; minSize: integer; callback : TGSearchCallback );
    procedure DeleteNodes( nodeID : TGPageID );
  protected
    procedure InternalLoad( aStream : TMemoryStream ); override;
    procedure InternalSave( aStream : TMemoryStream ); override;
    function CountIndexNodes( nodeID : TGPageID ) : Cardinal;
  public
    constructor Create(SysRoot: TGSDBSysRoot; const name : String); override;

    function Count: Cardinal; override;
    procedure Clear; override;

    procedure Insert( const mer : TGMER; objID : TGPrimaryKey ); override;
    procedure Delete( const mer : TGMER; objID : TGPrimaryKey ); override;
    function IndexMER : TGMER; override;

    procedure Search( const searchMER : TGMER; minSize: integer; resultList : TList ); override;
    procedure Search( const searchMER : TGMER; minSize: integer; callback : TGSearchCallback ); override;

    procedure SaveToXML( el : TGXML_Element ); override;

    property Header: TGRTreeHeader read FHeader;
  end;

procedure InitialiseRTree( pageSize : integer );

implementation

uses GLogging;

//-------------------------------------------------------------------------------
//Description
//Called when the spatial database is first opened, it gives the RTree class an
//\opportunity to set it's data structures to the same size as used by the spatial
//database.
//-------------------------------------------------------------------------------
procedure InitialiseRTree( pageSize : integer );
begin
  RTREE_MAX_ENTRIES := ( pageSize - Sizeof(TGRTreePageRec)) div SizeOf(TGRTreeTupleRec);
  RTREE_MIN_ENTRIES := RTREE_MAX_ENTRIES div 4;
end;

{ TGRTreeIndex }

procedure TGRTreeIndex.AdjustTree( nodeID, splitID: TGPageID );
var
  node, nodeParent, nodeSplit : TGRTreeNode;
  parentID : TGPageID;
  idx : integer;
begin
  // If at the root node then stop
  while nodeID <> FHeader.RootNodeID do
  begin
    node := TGRTreeNode.Create( SysRoot.PageCache, nodeID );
    parentID := node.ParentID;
    nodeParent := TGRTreeNode.Create( SysRoot.PageCache, parentID );
    nodeParent.IsLeaf := false;

    try
      // locate parent entry to this node
      idx := nodeParent.IndexOf(nodeID);
      if idx < 0 then
        Assert( idx >= 0, 'AdjustTree: Cannot find node in Parent ' + RowIDToStr(nodeID) );
      nodeParent.UpdateTuple(idx, node.NodeMER, nodeID );

      if splitID <> 0 then
      begin
        nodeSplit := TGRTreeNode.Create( SysRoot.PageCache, splitID );

        // if there is space in the parent node add the split node
        if nodeParent.Count < RTREE_MAX_ENTRIES then
        begin
          nodeParent.AddTuple( nodeSplit.NodeMER, splitID );
          nodeSplit.ParentID := parentID;
          splitID := 0;
        end
        else  // split the parent node
        begin
          SplitID := SplitNode( parentID, nodeSplit.NodeMER, splitID);
          // set the ParentID of the split node depending on where it ended up
          if nodeParent.IndexOf( nodeSplit.PageID ) < 0 then
            nodeSplit.ParentID := SplitID
          else
            nodeSplit.ParentID := parentID;
        end;
        nodeSplit.Free;
      end;
      nodeID := parentID;
    finally
      nodeParent.Free;
      node.Free;
    end;
  end;

  // if we have split the node and it is the Root node then grow the tree
  if ( splitID <> 0 ) and ( nodeID = FHeader.RootNodeID ) then
    GrowTree( splitID )
end;

//-----------------------------------------
//Description
//Removes all entries from the RTree index.
//-----------------------------------------
procedure TGRTreeIndex.Clear;
begin
  DeleteNodes(FHeader.RootNodeID);
  FHeader.PageCount := 0;
  FHeader.TreeDepth := 1;
  FHeader.RootNodeID := NewNode;
  FIndexMERValid := False;
  Modified := True;
end;

function TGRTreeIndex.ChooseLeaf( nodeID: TGPageID; mer : TGMER ) : TGPageID;
var
  node : TGRTreeNode;
  idx : integer;
  leastArea, area : Extended;
  leastID : TGPageID;
  tmpMER : TGMER;
begin
  node := TGRTreeNode.Create( SysRoot.PageCache, nodeID );

  if node.IsLeaf then
    Result := nodeID
  else
  begin
    leastArea := MaxDouble;
    leastID := 0;

    if node.Count = 0 then
      Assert( node.Count > 0, 'ChooseLeaf: Node has no entries' );

    // Find the node that needs to be expanded the least to include the mer
    for idx := 0 to node.Count - 1 do
    begin
      tmpMER := MER_Union( node[idx].MER, mer );

      area := MER_Area( tmpMER );

      if area < leastArea then
      begin
        leastArea := area;
        leastID := node[idx].RefID;
      end;
    end;
    Result := ChooseLeaf( leastID, mer );
  end;
  node.Free;
end;

procedure TGRTreeIndex.CondenseTree( nodeID: TGPageID; var list : TGPageIDArray );
var
  node, nodeParent : TGRTreeNode;
begin
  while nodeID <> FHeader.RootNodeID do
  begin
    node := TGRTreeNode.Create( SysRoot.PageCache, nodeID );
    nodeParent := TGRTreeNode.Create( SysRoot.PageCache, node.ParentID );

    if node.Count < RTREE_MIN_ENTRIES then
    begin
      // Add node to the re-insert set
      SetLength( list, Length(list ) + 1 );
      list[High(list)] := nodeID;

      // delete this node from the parent node
      nodeParent.DeleteTuple(nodeID);
      node.Free;
    end
    else
    begin
      // update the parents reference to this node
      nodeParent.UpdateTuple(nodeParent.IndexOf(nodeID), node.NodeMER, nodeID);
      node.Free;
    end;

    nodeID := nodeParent.PageID;
    nodeParent.Free;
  end;
end;

function TGRTreeIndex.Count: Cardinal;
begin
  Result := FNodeCount;
end;

function TGRTreeIndex.CountIndexNodes( nodeID : TGPageID ) : Cardinal;
var
  node : TGRTreeNode;
  idx : integer;
begin
  node := TGRTreeNode.Create( SysRoot.PageCache, nodeID );
  if node.IsLeaf then
    Result := node.Count
  else
  begin
    Result := 0;
    for idx := 0 to node.Count - 1 do
      Result := Result + CountIndexNodes( node.GetTuple(idx).RefID );
  end;
  node.Free;
end;

//--------------------------------------
//Description
//Deletes an entry from the RTree index.
//--------------------------------------
procedure TGRTreeIndex.Delete(const mer : TGMER; objID: TGPrimaryKey);
var
  leafID : TGPageID;
  node : TGRTreeNode;
  idx : integer;
  list : TGPageIDArray;
begin
  FIndexMERValid := False;
  leafID := FindLeaf( FHeader.RootNodeID, mer, objID );

  if leafID = 0 then
    Exit;

  node := TGRTreeNode.Create( SysRoot.PageCache, leafID );
  node.DeleteTuple(objID);
  node.Free;
  Dec( FNodeCount );

  CondenseTree( leafID, list );

  // Re-insert all the nodes in the list
  for idx := 0 to High( list ) do
    ReInsertNode( list[idx] );

  // Shrink the tree if there is only one node in the root
  ShrinkTree;
end;

procedure TGRTreeIndex.DeleteNodes(nodeID: TGPageID);
var
  node : TGRTreeNode;
  idx : integer;
begin
  node := TGRTreeNode.Create( SysRoot.PageCache, nodeID );

  // delete all the sub nodes
  if not node.isLeaf then
    for idx := 0 to node.Count - 1 do
      DeleteNodes( node[idx].RefID );

  node.Free;
  SysRoot.PageCache.DeletePage(nodeID);
  Dec( FHeader.PageCount );
  Modified := True;
end;

function TGRTreeIndex.FindLeaf( nodeID: TGPageID; const mer : TGMER; refID : TGPrimaryKey) : TGPageID;
var
  node : TGRTreeNode;
  idx : integer;
begin
  node := TGRTreeNode.Create( SysRoot.PageCache, nodeID );
  try
    if node.IsLeaf then
    begin
      for idx := 0 to node.Count - 1 do
        if node[idx].RefID = refID then
          if MER_IsEqual( mer, node[idx].MER ) then
          begin
            Result := nodeID;
            Exit;
          end;
    end
    else
      for idx := 0 to node.Count - 1 do
        if MER_Intersect( mer, node[idx].MER ) then
        begin
          Result := FindLeaf( node[idx].RefID, mer, refID );
          if Result <> 0 then
            Exit;
        end;
  finally
    node.Free;
  end;

  Result := 0;
end;

procedure TGRTreeIndex.GrowTree(splitID: TGPageID);
var
  root, node, split : TGRTreeNode;
begin
  // Create a new root node
  root := TGRTreeNode.Create( SysRoot.PageCache, NewNode );
  root.IsLeaf := false;

  // Add the current root node
  node := TGRTreeNode.Create( SysRoot.PageCache, FHeader.RootNodeID );
  node.ParentID := root.PageID;
  root.AddTuple( node.NodeMER, FHeader.RootNodeID );

   // add in the split node
  split := TGRTreeNode.Create( SysRoot.PageCache, splitID );
  split.ParentID := root.PageID;
  root.AddTuple( split.NodeMER, splitID );

  FHeader.RootNodeID := root.PageID;  // update the pointer to the Root node
  Inc( FHeader.TreeDepth );
  split.Free;
  node.Free;
  root.Free;
  Modified := True;
end;

//------------------------------------------
//Description
//Inserts an new entry into the RTree index.
//------------------------------------------
procedure TGRTreeIndex.Insert( const mer: TGMER; objID: TGPrimaryKey);
var
  leafID, splitID : TGPageID;
  node : TGRTreeNode;
begin
  FIndexMERValid := False;
  splitID := 0;
  leafID := ChooseLeaf( FHeader.RootNodeID, mer );

  node := TGRTreeNode.Create( SysRoot.PageCache, leafID );

  if node.Count < RTREE_MAX_ENTRIES then
  begin
    node.AddTuple( mer, objID );
    Inc( FNodeCount );
  end
  else
    splitID := SplitNode( leafID, mer, objID );

  node.Free;

  AdjustTree( leafID, splitID );
  Modified := True;
end;

function TGRTreeIndex.NewNode: TGPageID;
var
  node : TGRTreeNode;
begin
  Result := SysRoot.PageCache.NewPage;
  Inc( FHeader.PageCount );
  Modified := True;

  node := TGRTreeNode.Create( SysRoot.PageCache, Result );
  node.IsLeaf := True;
  node.Count := 0;
  node.Free;
end;

//-------------------------------------------------------------------------
//Description
//Used to load the properties of the RTree index from the spatial database.
//-------------------------------------------------------------------------
procedure TGRTreeIndex.InternalLoad(aStream: TMemoryStream);
begin
  aStream.Read( FHeader, SizeOf( TGRTreeHeader ));

  if FHeader.Magic >= RTREE_MAGIC1 then
    FIndexMER := FHeader.IndexMER
  else
    MER_Empty(FIndexMER);

  FIndexMERValid := not MER_IsEmpty( FIndexMER );

  if FHeader.Magic >= RTREE_MAGIC then
    Exit;

  Assert( FHeader.Magic = RTREE_MAGIC, 'RTree Magic Failed' );
end;

//-------------------------------------------------------------------------
//Description
//Used to save the properties of the RTree index from the spatial database.
//-------------------------------------------------------------------------
procedure TGRTreeIndex.InternalSave(aStream: TMemoryStream);
begin
  // Create a root page if this index is empty
  if FHeader.RootNodeID = 0 then
  begin
    FHeader.RootNodeID := NewNode;
    Modified := True;
  end;

  FHeader.Magic := RTREE_MAGIC1;
  FHeader.IndexMER := IndexMER;
  aStream.Write( FHeader, SizeOf( TGRTreeHeader ));
end;

procedure TGRTreeIndex.PickSeeds(var list: TGRTreeTupleArray; node, split: TGRTreeNode);
var
  idx : integer;
  hlsV : integer;  // Highest Low Side - Vertical
  lhsV : integer;  // Lowest High Side - Vertical
  hlsH : integer;  // Highest Low Side - Horisontal
  lhsH : integer;  // Lowest High Side - Horisontal

  diffH : Double;
  diffV : Double;
begin
  // Select 2 entries to be the first elements of the nodes.
  hlsV := 0;
  hlsH := 0;

  // find the Highest Low Side and Lowest High Side pairs in both dimensions
  for idx := 0 to High( list ) do
    with list[idx] do
    begin
      if MER_iTopY(MER) > MER_iTopY( list[hlsV].MER ) then
        hlsV := idx;
      if MER.iLongX > list[hlsH].MER.iLongX then
        hlsH := idx;
    end;

  if hlsV = 0 then
    lhsV := 1
  else
    lhsV := 0;

  if hlsH = 0 then
    lhsH := 1
  else
    lhsH := 0;

  // make sure the same tuple does not get picked again
  for idx := 0 to High( list ) do
    with list[idx] do
    begin
      if ( idx <> hlsV ) and ( MER.iLatY < list[lhsV].MER.iLatY ) then
        lhsV := idx;
      if ( idx <> hlsH ) and ( MER_iRightX(MER) < MER_iRightX(list[lhsH].MER)) then
        lhsH := idx;
    end;

    Assert( lhsH <> hlsH );
    Assert( lhsV <> hlsV );

    // get the seperations
    diffH := list[lhsH].MER.iHeightY;
    diffV := list[hlsV].MER.iWidthX;

    with node.NodeMER do
    begin
      node.Count := 0;
      split.Count := 0;

      // compare the normalised differences
      if ( diffH / ( iWidthX )) > ( diffV / ( iHeightY )) then
      begin
        // use the horisontal pair
        node.AddTuple( list[hlsH].MER, list[hlsH].RefID );
        split.AddTuple( list[lhsH].MER, list[lhsH].RefID );
      end
      else
      begin
        // use the vertical pair
        node.AddTuple( list[hlsV].MER,list[hlsV].RefID );
        split.AddTuple( list[lhsV].MER,list[lhsV].RefID );
      end;
    end;
end;

procedure TGRTreeIndex.ReInsertNode( nodeID : TGPageID );
var
  node : TGRTreeNode;
  idx : integer;
begin
  //ToDo: this routine could be much more efficient
  node := TGRTreeNode.Create( SysRoot.PageCache, nodeID );

  // if a leaf node just insert the tuples
  if node.IsLeaf then
  begin
    for idx := 0 to node.Count - 1 do
      Insert( node[idx].MER, node[idx].RefID );
  end
  else  // Now re-insert the non-leaf nodes
    for idx := 0 to node.Count - 1 do
      ReInsertNode( node[idx].RefID );

  node.Free;
  SysRoot.PageCache.DeletePage(nodeID);  // drop the page from the cache
  Dec( FHeader.PageCount );
  Modified := True;
end;

//---------------------------------------------------
//Description
//\Returns the maximum MER that the index represents.
//---------------------------------------------------
function TGRTreeIndex.IndexMER: TGMER;
var
  node : TGRTreeNode;
begin
  if not FIndexMERValid and ( FHeader.RootNodeID <> 0 ) then
  begin
    node := TGRTreeNode.Create( SysRoot.PageCache, FHeader.RootNodeID );
    FIndexMER := node.NodeMER;
    node.Free;
    FIndexMERValid := true;
    FNodeCount := CountIndexNodes(FHeader.RootNodeID);
  end;

  Result := FIndexMER;
end;

//----------------------------------------------------------
//Description
//Saves the properties of the RTree index to an XML element.
//----------------------------------------------------------
procedure TGRTreeIndex.SaveToXML(el: TGXML_Element);
begin
  inherited;

  el.AddAttribute( 'PageCount', FHeader.PageCount, -1 );
  el.AddAttribute( 'Depth', FHeader.TreeDepth, -1 );
end;

//-----------------------------------------------------------------------------
//Description
//Searches for objects that intersect the SearchMER and adds the primary key of
//the object to a list when an object is found.
//
//Parameters
//searchMER :   The MER to search with
//minSize :     Objects smaller than this value are ignored
//resultList :  The list to add the primary key to.
//-----------------------------------------------------------------------------
procedure TGRTreeIndex.Search( const searchMER: TGMER; minSize: integer; resultList : TList);
begin
  resultList.Clear;
  FResultList := resultList;
  SearchTree( searchMER, minSize, nil );
  FResultList := nil;
end;

//-------------------------------------------------------------------------------
//Description
//Searches for objects that intersect the SearchMER and calls a callback function
//when an object is found.
//
//Parameters
//searchMER :  The MER to search with
//minSize :    Objects smaller than this are ignored.
//callback :   The callback routine to call when an object is found.             
//-------------------------------------------------------------------------------
procedure TGRTreeIndex.Search( const searchMER: TGMER; minSize: integer; callback: TGSearchCallback);
begin
  FAbortSearch := false;
  SearchTree( searchMER, minSize, callback );
end;

procedure TGRTreeIndex.SearchNode(nodeID: TGPageID; minSize : integer; const searchMER : TGMER; callback : TGSearchCallback);
var
  idx : integer;
  node : TGRTreeNode;
begin
  if FAbortSearch  or ( nodeID = 0 ) then
    Exit;

  node := TGRTreeNode.Create( SysRoot.PageCache, nodeID );

  if node.IsLeaf then // search the leaf nodes.
  begin
    if Assigned( callback ) then
    begin
      for idx := 0 to node.Count - 1 do
        with node[idx]^ do
          if ( minSize = 0 ) or ( MER.iWidthX > minSize ) or ( MER.iHeightY > minSize ) or (( MER.iWidthX = 1 ) and ( MER.iHeightY = 1 )) then
            if MER_Intersect(searchMER, MER) then
            begin
              callback( RefID, FAbortSearch );
              if FAbortSearch then
                Break;
            end;
    end
    else
      for idx := 0 to node.Count - 1 do
        with node[idx]^ do
          if ( minSize = 0 ) or ( MER.iWidthX > minSize ) or ( MER.iHeightY > minSize ) or (( MER.iWidthX = 1 ) and ( MER.iHeightY = 1 )) then
            if MER_Intersect(searchMER, MER) then
              FResultList.Add( Pointer( RefID ));  // Add all of the overlapping objects to the list
  end
  else  // recursively search a non-leaf node
    for idx := 0 to node.Count - 1 do
      with node[idx]^ do
        if ( minSize = 0 ) or ( MER.iWidthX > minSize ) or ( MER.iHeightY > minSize ) then
          if MER_Intersect(searchMER, MER) then
          begin
            SearchNode( RefID, minSize, searchMER, callback );    // Recursively search all of the non-leaf nodes

            if FAbortSearch then
              Break;
          end;

  node.Free;
end;

procedure TGRTreeIndex.SearchTree( searchMER : TGMER; minSize: integer; callback : TGSearchCallback );
begin
  // Check to see if this layer is valid at all
  with IndexMER do
    if (iWidthX = 1) or (iHeightY = 1) or (iWidthX > minSize) or (iHeightY > minSize) then
      if MER_Intersect( IndexMER, searchMER ) then
      begin
{        if searchMER.Crosses180 then
        begin
          with searchMER do
            SearchNode( FHeader.RootNodeID, minSize, MER( -GU_180_DEGREE, iLatY, iWidthX - ( GU_180_DEGREE - iLongX ), iHeightY ), callback );

          with searchMER do
            searchMER := MER( iLongX, iLatY, GU_180_DEGREE - iLongX, iHeightY );
        end;
}
        SearchNode( FHeader.RootNodeID, minSize, searchMER, callback );
      end;
end;

procedure TGRTreeIndex.ShrinkTree;
var
  newRootID : TGPageID;
  root : TGRTreeNode;
begin
  newRootID := 0;
  root := TGRTreeNode.Create( SysRoot.PageCache, FHeader.RootNodeID );

  // if not a leaf node and there is only one entry in the root
  if ( not root.IsLeaf ) and ( root.Count = 1 ) then
    newRootID := root[0].RefID;

  root.Free;

  // shrink the tree
  if newRootID <> 0 then
  begin
    SysRoot.PageCache.DeletePage(FHeader.RootNodeID);
    Dec( FHeader.PageCount );
    FHeader.RootNodeID := newRootID;
    Dec( FHeader.TreeDepth );
    root := TGRTreeNode.Create( SysRoot.PageCache, FHeader.RootNodeID );
    root.ParentID := 0;
    root.Free;
    Modified := True;
  end;
end;

function TGRTreeIndex.SplitNode( nodeID: TGPageID; const mer : TGMER; refID : TGPrimaryKey ) : TGPageID;
var
  node, split : TGRTreeNode;
  idx : integer;
  list : TGRTreeTupleArray;
begin
  node := TGRTreeNode.Create( SysRoot.PageCache, nodeID );
  Result := NewNode;
  split := TGRTreeNode.Create( SysRoot.PageCache, Result );
  split.IsLeaf := node.IsLeaf;

  // build up the list of all Tuples including the new Tuple
  SetLength( list, node.Count + 1 );
  for idx := 0 to node.Count - 1 do
    list[idx] := node[idx]^;
  list[node.Count].MER := mer;
  list[node.Count].RefID := refID;

  PickSeeds( list, node, split );

  // assign the rest of the tuples to the pages
  for idx := 0 to High( list ) do
    with list[idx] do
      if ( node.IndexOf( RefID ) < 0 ) and ( split.IndexOf( RefID ) < 0 ) then
      begin
        if ( idx mod 2 ) = 0 then
          node.AddTuple(MER, RefID)
        else
          split.AddTuple(MER, RefID);
      end;

  node.Free;
  if not split.IsLeaf then
    for idx := 0 to split.Count - 1 do
    begin
      node := TGRTreeNode.Create( SysRoot.PageCache, split[idx].RefID );
      node.ParentID := split.PageID;
      node.Free;
    end;

  split.Free;
  SetLength( list, 0 );
end;

constructor TGRTreeIndex.Create(SysRoot: TGSDBSysRoot; const name: String);
begin
  inherited;

  FNodeCount := MAXDWORD;
  IndexMER; // make sure the Index MER is calculated
end;

{ TGRTreeNode }

procedure TGRTreeNode.AddTuple(mer: TGMER; refID: TGPrimaryKey);
var
  idx : integer;
begin
  idx := Count;

  Assert( idx < RTREE_MAX_ENTRIES, 'AddTuple: Failed node full' );

  Count := idx + 1;
  UpdateTuple( idx, mer, refID );
end;

constructor TGRTreeNode.Create(pageCache: TGPageCache; pageID: TGPageID);
begin
  inherited;
  
  FNodePtr := PTGRTreeNodeRec( DataPtr );
end;

procedure TGRTreeNode.DeleteTuple(refID: TGPrimaryKey);
var
  idx : integer;
begin
  idx := IndexOf( refID );

  Assert( idx >= 0, 'DeleteTuple: Bad refID passed' );

  // delete the entry
  if idx < Count - 1 then
    Move( Tuple[idx + 1]^, Tuple[idx]^, SizeOf( TGRTreeTupleRec ) * ( Count - ( idx + 1 )));

  Count := Count - 1;
end;

function TGRTreeNode.GetCount: integer;
begin
  Result := FNodePtr.Header.NodeCount;
end;

function TGRTreeNode.GetIsLeaf: boolean;
begin
  Result := FNodePtr.Header.LeafNode;
end;

function TGRTreeNode.GetParentID: TGPageID;
begin
  Result := FNodePtr.Header.ParentID;
end;

function TGRTreeNode.GetTuple(index: integer): PTGRTreeTupleRec;
begin
//  Assert( index < Count, 'Bad GetTuble Index ' + IntToStr( index ));
  Result := @FNodePtr.Tuples[index];
end;

function TGRTreeNode.IndexOf(refID: TGPrimaryKey): integer;
var
  idx : integer;
begin
  for idx := 0 to Count - 1 do
    if Tuple[idx].RefID = refID then
    begin
      Result := idx;
      Exit;
    end;

  Result := -1;
end;

function TGRTreeNode.NodeMER: TGMER;
var
  idx : integer;
begin
  // Calculates the Union of all the MERs in the node
  Result := Tuple[0].MER;
  for idx := 1 to Count - 1 do
    Result := MER_Union( Result, Tuple[idx].MER );
end;

procedure TGRTreeNode.SetCount(const Value: integer);
begin
  Assert( Value <= RTREE_MAX_ENTRIES, 'Count exceeds Maximum entries' );
  FNodePtr.Header.NodeCount := Value; // update the count
  Modified := True;
end;

procedure TGRTreeNode.SetIsLeaf(const Value: boolean);
begin
  FNodePtr.Header.LeafNode := Value;
  Modified := True;
end;

procedure TGRTreeNode.SetParentID(const Value: TGPageID);
begin
  FNodePtr.Header.ParentID := Value;
  Modified := True;
end;

procedure TGRTreeNode.SetTuple(index: integer;
  const Value: PTGRTreeTupleRec);
begin
  Assert( index < Count, 'Bad GetTuble Index ' + IntToStr( index ));

  FNodePtr.Tuples[index].MER := Value.MER;
  FNodePtr.Tuples[index].RefID := Value.RefID;

  Modified := True;
end;

procedure TGRTreeNode.UpdateTuple(idx: integer; mer: TGMER; refID: TGPrimaryKey);
begin
  Assert(( idx >= 0 ) and ( idx < Count ), 'UpdateTuple: Invalid Index passed' );

  Tuple[idx].MER := mer;
  Tuple[idx].RefID := refID;
  
  Modified := True;
end;

initialization
  RegisterSDBClass( TGRTreeIndex, '' );
end.
