//-----------------------------------------------------------------------
// Summary
//	Spatial Database Btree classes
//
// Description
//   Modified and bug fixed from some open source found on the net
//   I have lost the original source so if anyone recognises the
//   code please let me know so that I can properly credit them.
//
// Author
// 	Graham Knight (tglobe@tglobe.com)
//-----------------------------------------------------------------------
{$I GLOBE5.INC}

unit GSDBBTree;

interface

uses
  Windows, Classes, SysUtils, GSDBCache, GSysUtils, GSDBTables, GXML;
const
  BTREE_MAGIC = $42540000;   
type
  TBTreePageHeader = packed record
    ItemCount: integer; // ItemCount of items 1n the Page
    LeftID: TGPageID; // btree prior Page ID
//    ParentID: TGPageID; // btree parent Page ID (do not use!)
  end;
  PTBTreePageHeader = ^TBTreePageHeader;

  TBTreeItem = packed record
    RightID: TGPageID; // descendant Btree Page PageID
    DataRowID: TGRowID; // Page to associate with key
    KeyID: TGPrimaryKey; // ID to locate entry with - must be unique
  end;
  PTBTreeItem = ^TBTreeItem;

//const
//  MAX_ITEMS = ( xCACHE_PAGE_SIZE - SizeOf(TBTreePageHeader)) div SizeOf(TBTreeItem);
//  HALF_FULL = MAX_ITEMS div 2;
var
  MAX_ITEMS : integer = 0;
  HALF_FULL : integer = 0;

type

  TGBTreePage = class(TGPage)
  private
    FPgHead: PTBTreePageHeader;
    function GetItem(index: integer): PTBTreeItem;
    procedure SetItem(index: integer; const Value: PTBTreeItem);
    function GetItemCount: integer;
    function GetLeftID: TGRowID;
    procedure SetItemCount(const Value: integer);
    procedure SetLeftID(const Value: TGRowID);
  public
    constructor Create( pageCache: TGPageCache; pageID : TGPageID ); override;
    property ItemCount : integer read GetItemCount write SetItemCount;
    property LeftID : TGRowID read GetLeftID write SetLeftID;
    property Item[index:integer] : PTBTreeItem read GetItem write SetItem; default; // array of Items held on the Page
  end;

  // Header record for an BTree index
  TGBTreeHeader = packed record
    Magic : DWORD;
    RootNodeID : TGPageID;
    TreeDepth : integer;
    PageCount : integer;
    TotalItems : integer;
  end;

  TGBTreeIndex = class( TGSDBObject )
  private
    FHeader : TGBTreeHeader;
    FRootID : TGRowID;

    function NewNode: TGPageID;
    procedure DeletePage(aPageID: TGPageID);

    procedure InsertItem(var Item: TBTreeItem; branchID: TGPageID; index: integer; var grown: Boolean);
    function AddItem(var Item: TBTreeItem; branchID: TGPageID; var grown: Boolean): Boolean;
    function LocateItem(aKeyID: TGPrimaryKey; branchID: TGPageID; var index: integer): TGPageID;
    function DeleteItem(aKeyID: TGPrimaryKey; branchID: TGPageID; var shrunk: Boolean): Boolean;
    function FindItem(aKeyID: TGPrimaryKey; node: TGBTreePage; var index: integer): Boolean;

    procedure BalanceRight(position: integer; childID: TGPageID; RightID: TGPageID; parentID: TGPageID; right_extras: integer; var shrunk: Boolean);
    procedure BalanceLeft(position: integer; childID: TGPageID; leftID: TGPageID; parentID: TGPageID; left_extras: integer; var shrunk: Boolean);
    procedure FindPredecessor(index: integer; descendID: TGPageID; branchID: TGPageID; var shrunk: Boolean);
    procedure MergeLeft(position: integer; leftID: TGPageID; childID: TGPageID; parentID: TGPageID; var shrunk: Boolean);
    procedure MergeRight(position: integer; RightID: TGPageID; childID: TGPageID; parentID: TGPageID; var shrunk: Boolean);
    procedure SplitNode(var Item: TBTreeItem; branchID: TGPageID; position: integer);
    procedure UnderFlow(parentID: TGPageID; childID: TGPageID; position: integer; var shrunk: Boolean);
  protected
    procedure DeleteNodes( nodeID : TGPageID );

    procedure InternalLoad( aStream : TMemoryStream ); override;
    procedure InternalSave( aStream : TMemoryStream ); override;
  public
    procedure Clear; override;

    procedure Add(aKeyID: TGPrimaryKey; DataID: TGPageID);
    function Delete(aKeyID: TGPrimaryKey): Boolean;
    function FindKey(aKeyID: TGPrimaryKey; var Item: TBTreeItem): Boolean;
    function Find(aKeyID: TGPrimaryKey ) : TGPrimaryKey;

    procedure SaveToXML( el : TGXML_Element ); override;

    property RootID: TGPageID read FRootID;
  end;

procedure InitialiseBTree( pageSize : integer );

implementation

//-------------------------------------------------------------------------------
//Description
//Called when the spatial database is first opened, it gives the BTree class an
//\opportunity to set it's data structures to the same size as used by the spatial
//database.                                                                      
//-------------------------------------------------------------------------------
procedure InitialiseBTree( pageSize : integer );
begin
  MAX_ITEMS := ( pageSize - SizeOf(TBTreePageHeader)) div SizeOf(TBTreeItem);
  HALF_FULL := MAX_ITEMS div 2;
end;

{-------------------------------------------------------------------------
  TGBTreeIndex.NewNode
-------------------------------------------------------------------------}
{** To allocate a new PageID to become part of the B-tree.
  @Param None
  @Result TGPageID
}
function TGBTreeIndex.NewNode: TGPageID;
var
  page: TGBTreePage; {the newly created extra Page}
begin
  Result := SysRoot.PageCache.NewPage;
  Inc( FHeader.PageCount );
  Modified := True;

  page := TGBTreePage.Create( SysRoot.PageCache, Result );
  page.ItemCount := 0;
  page.LeftID := 0;
  page.Free;
end;

{-------------------------------------------------------------------------
  TGBTreeIndex.DeletePage
-------------------------------------------------------------------------}
{** To free a PageID that is no longer needed.

  @Param aPageID: TGPageID
  @Result None
}
procedure TGBTreeIndex.DeletePage(aPageID: TGPageID);
begin
  SysRoot.PageCache.DeletePage(aPageID);
end;

{-------------------------------------------------------------------------
  TGBTreeIndex.FindItem
-------------------------------------------------------------------------}
{** Locates an item in a Page or the location that the item should be at.

  @Param aKeyID : TGPrimaryKey;
  @Param const node: PTBTreePage;
  @Param var found : Boolean
  @Result integer
}
function TGBTreeIndex.FindItem(aKeyID: TGPrimaryKey; node: TGBTreePage; var index: integer): Boolean;
var
  L, H, I: Integer;
  nodeKeyID: TGPrimaryKey;
begin
  Result := False;
  L := 1;
  H := node.ItemCount;
  while L <= H do
  begin
    I := (L + H) shr 1;
    nodeKeyID := node.Item[I].KeyID;
    if aKeyID > nodeKeyID then
      L := I + 1
    else
    begin
      H := I - 1;
      if aKeyID = nodeKeyID then
      begin
        Result := True;
        L := I;
      end;
    end;
  end;
  index := L;
end;

{-------------------------------------------------------------------------
  TGBTreeIndex.Split
-------------------------------------------------------------------------}
{** To Split a Page that has grown too large.

  @Param var Item: TBTreeItem;
  @Param branchID: TGPageID;
  @Param position: integer;
  @Result None

  First build an oversize Page with all the TBTreeItem in it.
  The bottom half of this big Page will remain in the current Page.
  The middle Item from the big Page will be promoted to the parent.
  The top half of the big Page will be moved into a new Page.
}
procedure TGBTreeIndex.SplitNode(var Item: TBTreeItem; branchID: TGPageID;
  position: integer);
type
  Tbig_Page = packed record
    ItemCount: integer;
    Item: array[1..1] of TBTreeItem;
  end;
var
  middle: integer; {the ItemCount of the Item to be promoted}
  big_Page: Tbig_Page; {the oversize Page}
  new_Page: TGBTreePage; {the newly created extra Page}
  size: integer; {the ItemCount of TBTreeItem in the new Page}
  branch: TGBtreePage;
begin
  branch := TGBtreePage.Create(SysRoot.PageCache, branchID);

  // create the Page I would like to have...
  Move(branch[1]^, big_Page.Item[1], SizeOf(TBTreeItem) * (position - 1));

  big_Page.Item[position] := Item;
  if position <= MAX_ITEMS then
    Move(branch[position]^, big_Page.Item[position + 1], SizeOf(TBTreeItem) * (1 + branch.ItemCount - position));
  big_Page.ItemCount := branch.ItemCount + 1;

  //set up the Item to be promoted to the parent
  middle := MAX_ITEMS div 2 + 1; {Item ItemCount to promote}
  Item := big_Page.Item[middle]; {Item to be promoted}
  Item.RightID := NewNode; {right child will be new Page}

  // set up the old Page
  Move(big_Page.Item[1], branch[1]^, SizeOf(TBTreeItem) * (middle - 1));
  branch.ItemCount := middle - 1;

  // set up the new Page...
  new_Page := TGBtreePage.Create(SysRoot.PageCache, Item.RightID);
  new_Page.LeftID := big_Page.Item[middle].RightID;

  size := 1 + big_Page.ItemCount - (middle + 1);
  Move(big_Page.Item[middle + 1], new_Page[1]^, SizeOf(TBTreeItem) * size);
  new_Page.ItemCount := size;

  // output the modified Pages
  new_Page.Free;
  branch.Free;
end;

{-------------------------------------------------------------------------
  TGBTreeIndex.Insert
-------------------------------------------------------------------------}
{** To Insert an Item into the current Page.

  @Param var Item: TBTreeItem; the Item to be Added to the current Page
  @Param branchID: TGPageID; the Page ID of the current Page
  @Param index: integer; the position within the current Page for Item
  @Param var grown: Boolean; True if (sub-)tree has grown higher
  @Result None
}
procedure TGBTreeIndex.InsertItem(var Item: TBTreeItem; branchID: TGPageID;
  index: integer; var grown: Boolean);
var
  branch: TGBTreePage;
begin
  branch := TGBtreePage.Create(SysRoot.PageCache, branchID);

  if branch.ItemCount < MAX_ITEMS then
  begin
    // room for new Item in this Page
    grown := FALSE;
    Move(branch[index]^, branch[index + 1]^, SizeOf(TBTreeItem) * (1 + branch.ItemCount - index));
    branch.ItemCount := branch.ItemCount + 1;
    branch[index]^ := item;
    FHeader.TotalItems := FHeader.TotalItems + 1;
    Modified := True;
  end
  else // no room so Split the Page...
    SplitNode(Item, branchID, index);

  branch.Free;
end;

(*
 * Name:    Add
 * Purpose: To Add an Item somewhere in the current "branch" of the B-tree.
 * Passed:  Item:      the Item to be Added to the current Page
 *          branchID: the Page ID of the current Page
 * Returns: Item:      the Item to be promoted if a Split occurred
 *          grown:     True if (sub-)tree has grown higher
 *)
function TGBTreeIndex.AddItem(var Item: TBTreeItem; branchID: TGPageID;
  var grown: Boolean): Boolean;
var
  branch: TGBTreePage; {the current Page}
  index: integer; {the position for Item within branch}
begin
  if branchID = 0 then
  begin
    // Reached beyond the leaf of the tree, so Add Item into the previous level.
    grown := True;
    Result := True;
  end
  else
  begin
    branch := TGBtreePage.Create(SysRoot.PageCache, branchID);

    if (branch.ItemCount = 0) or (Item.KeyID > branch.Item[branch.ItemCount].KeyID) then
    begin
      // the new Item belongs after the final Item in this Page
      index := branch.ItemCount + 1;
      Result := AddItem(Item, branch.Item[branch.ItemCount].RightID, grown);
      if grown then
        InsertItem(Item, branchID, index, grown);
    end
    else
    begin
      // scan the TBTreeItem in this Page for the correct position for the new Item
      if FindItem(Item.KeyID, branch, index) then
      begin
        // Duplicate Key found - ignore
        grown := False;
        Result := False;
      end
      else
      begin
        if index = 1 then
          Result := AddItem(Item, branch.LeftID, grown)
        else
          Result := AddItem(Item, branch.Item[index - 1].RightID, grown);
        if grown then
          InsertItem(Item, branchID, index, grown);
      end;
    end;
    branch.Free;
  end;
end;

(*
 * Name:    Add_KEY
 * Purpose: To Add a new KEY into the library
 *)
procedure TGBTreeIndex.Add(aKeyID: TGPrimaryKey; DataID: TGPageID);
var
  Item: TBTreeItem;
  grown: Boolean; {has the entire B-tree grown higher?}
  RootPage: TGBTreePage; {new root Page}
  PageID: TGPageID; {PageID of new root Page}
begin
  Item.KeyID := aKeyID;
  Item.DataRowID := DataID;
  Item.RightID := 0;

  AddItem(Item, FHeader.RootNodeID, grown);

  if grown then
  begin
    (*
     * This growth must be handled as a special case, because the
     * root is allowed to be less than half full.
     *)
    PageID := NewNode;

    RootPage := TGBtreePage.Create(SysRoot.PageCache, PageID);
    RootPage.ItemCount := 1;
    RootPage.LeftID := FHeader.RootNodeID;
    RootPage[1]^ := item;
    RootPage.Free;

    // Update the Old Root Page
    FHeader.RootNodeID := PageID;
    Modified := true;
  end;
end;

(*
 * Name:    BalanceRight
 * Purpose: To "borrow" extra TBTreeItem from a right sibling, to even out
 *           the distribution.
 * Passed:  position:     the position in parent separating siblings
 *          childID:     the Page ID of the current Page
 *          RightID:     the Page ID of the right sibling
 *          parentID:    the Page ID of the parent Page
 *          right_extras: the ItemCount of TBTreeItem available to the right
 * Returns: shrunk:       True if child is undersized
 *)
procedure TGBTreeIndex.BalanceRight(position: integer; childID: TGPageID;
  RightID: TGPageID; parentID: TGPageID; right_extras: integer; var shrunk: Boolean);
var
  right: TGBTreePage;
  child: TGBTreePage;
  parent: TGBTreePage;
begin
  right := TGBtreePage.Create(SysRoot.PageCache, RightID);
  child := TGBtreePage.Create(SysRoot.PageCache, ChildID);
  parent := TGBtreePage.Create(SysRoot.PageCache, ParentID);
  (*
   * copy all the TBTreeItem that must be moved from the right sibling
   *  into the current Page (child already has the Item from the
   *  parent), and adjust the size of the Page.
   *)
  Move(right[1]^, child[child.ItemCount + 1]^, SizeOf(TBTreeItem) * (right_extras - 1));
  child.ItemCount := HALF_FULL + right_extras - 1;
  shrunk := FALSE;

  // The largest "extra" Item from the right sibling becomes the new parent.
  parent.Item[position] := right.Item[right_extras];
  parent.Item[position].RightID := RightID;

  // fix up right sibling to shift everything left
  right.LeftID := right.Item[right_extras].RightID;
  right.ItemCount := right.ItemCount - right_extras;
  Move(right[right_extras + 1]^, right[1]^, SizeOf(TBTreeItem) * right.ItemCount);

  // output the modified Pages
  child.Free;
  Right.Free;
  parent.Free;
end;

(*
 * Name:    BalanceLeft
 * Purpose: To "borrow" extra TBTreeItem from a left sibling, to even out
 *           the distribution.
 * Passed:  position:     the position in parent separating siblings
 *          childID:     the Page ID of the current Page
 *          leftID:      the Page ID of the left sibling
 *          parentID:    the Page ID of the parent Page
 *          left_extras:  the ItemCount of TBTreeItem available to the left
 * Returns: shrunk:       True if child is undersized
 *)
procedure TGBTreeIndex.BalanceLeft(position: integer;
  childID: TGPageID; leftID: TGPageID;
  parentID: TGPageID; left_extras: integer; var shrunk: Boolean);
var
  left: TGBTreePage;
  child: TGBTreePage;
  parent: TGBTreePage;
begin
  left := TGBtreePage.Create(SysRoot.PageCache, LeftID);
  child := TGBtreePage.Create(SysRoot.PageCache, ChildID);
  parent := TGBtreePage.Create(SysRoot.PageCache, ParentID);

  // make room for TBTreeItem to come from left sibling
  Move(child[1]^, child[1 + left_extras]^, SizeOf(TBTreeItem) * (HALF_FULL - 1));

  // Insert the separating Item from the parent
  child.Item[left_extras] := parent.Item[position];
  child.Item[left_extras].RightID := child.LeftID;

  (*
   * The left sibling has now lost all but one of the "extras".
   *  The final extra will become the new separating Item in the parent.
   *)
  left.ItemCount := left.ItemCount - left_extras + 1;

  // move the "extras" from the left sibling to the current Page
  Move(left[left.ItemCount + 1]^, child[1]^, SizeOf(TBTreeItem) * (left_extras - 1));
  child.LeftID := left.Item[left.ItemCount].RightID;
  child.ItemCount := HALF_FULL - 1 + left_extras;
  shrunk := FALSE;

  // promote the first "extra" to the parent, and remove from left sibling
  parent.Item[position] := left.Item[left.ItemCount];
  parent.Item[position].RightID := childID;
  left.ItemCount := left.ItemCount - 1;

  // output the modified TBTreePage
  child.Free;
  left.Free;
  parent.Free;
end;

(*
 * Name:    MergeRight
 * Purpose: To merge the current Page with its right sibling, thus
 *           removing the right sibling from the B-tree.
 * Passed:  position:     the position in parent separating siblings
 *          RightID:     the Page ID of the right sibling
 *          childID:     the Page ID of the current Page
 *          parentID:    the Page ID of the parent Page
 * Returns: shrunk:       True if child is undersized
 *)
procedure TGBTreeIndex.MergeRight(position: integer;
  RightID: TGPageID; childID: TGPageID;
  parentID: TGPageID; var shrunk: Boolean);
var
  right: TGBTreePage;
  child: TGBTreePage;
  parent: TGBTreePage;
begin
  right := TGBtreePage.Create(SysRoot.PageCache, RightID);
  child := TGBtreePage.Create(SysRoot.PageCache, ChildID);
  parent := TGBtreePage.Create(SysRoot.PageCache, ParentID);
  (*
   * The current Page is one less that half full, and the right
   *  sibling is exactly half full.
   * Add all the TBTreeItem from the right sibling Page into the
   *  appropriate places in the current Page.
   *)
  Move(right[1]^, child[HALF_FULL + 1]^, SizeOf(TBTreeItem) * right.ItemCount);
  child.ItemCount := child.ItemCount + right.ItemCount;

  (*
   * shuffle parent TBTreeItem down to Delete the separating Item (the
   *  separating Item has already been placed in the current Page)
   * This may cause the parent Page to be undersized...
   *)
  Move(parent[position + 1]^, parent[position]^, SizeOf(TBTreeItem) * (1 + parent.ItemCount - 1 - position));
  parent.ItemCount := parent.ItemCount - 1;
  shrunk := parent.ItemCount < HALF_FULL;

  // output the modified Pages
  child.Free;
  parent.Free;

  //the right sibling can be re-used...
  DeletePage(RightID);
end;

(*
 * Name:    MergeLeft
 * Purpose: To merge the current Page with its left sibling, thus
 *           removing the left sibling from the B-tree.
 * Passed:  position:     the position in parent separating siblings
 *          leftID:      the Page ID of the left sibling
 *          childID:     the Page ID of the current Page
 *          parentID:    the Page ID of the parent Page
 * Returns: shrunk:       True if child is undersized
 * Notes:   The current Page must be the right-most child of the parent Page.
 *)
procedure TGBTreeIndex.MergeLeft(position: integer;
  leftID: TGPageID; childID: TGPageID; parentID: TGPageID; var shrunk: Boolean);
var
  left: TGBTreePage;
  parent: TGBTreePage;
  child: TGBTreePage;
begin
  left := TGBtreePage.Create(SysRoot.PageCache, LeftID);
  child := TGBtreePage.Create(SysRoot.PageCache, ChildID);
  parent := TGBtreePage.Create(SysRoot.PageCache, ParentID);

  // move the separating Item from the parent into the left sibling.
  left.ItemCount := left.ItemCount + 1;
  left.Item[left.ItemCount] := parent.Item[position];
  left.Item[left.ItemCount].RightID := child.LeftID;

  // move all the TBTreeItem from the current Page into the left sibling
  Move(child[1]^, left[left.ItemCount + 1]^, SizeOf(TBTreeItem) * child.ItemCount);
  left.ItemCount := left.ItemCount + child.ItemCount;

  (*
   * remove the separating Item from the parent. This is very simple,
   *  since merging is always done with the right sibling unless
   *  there is no right sibling, in which case the current Page must
   *  be the right-most child of the parent Page.
   *)
  parent.ItemCount := parent.ItemCount - 1;
  shrunk := parent.ItemCount < HALF_FULL;

  // output the modified Pages
  left.Free;
  parent.Free;

  // the current node can now be re-allocated
  DeletePage(childID);
end;

(*
 * Name:    UnderFlow
 * Purpose: To remedy the situation where the current Page has too few
 *           TBTreeItem in it. This may be done either by borrowing TBTreeItem
 *           from a sibling Page, or by merging two TBTreePage.
 * Passed:  parentID:    the Page ID of the parent Page
 *          childID:     the Page ID of the current Page
 *          position:     the position in parent separating siblings
 * Returns: shrunk:       True if child is undersized
 * Notes:   The current Page must be the right-most child of the parent Page.
 *)
procedure TGBTreeIndex.UnderFlow(parentID: TGPageID;
  childID: TGPageID; position: integer; var shrunk: Boolean);
var
  child: TGBTreePage; {current Page}
  parent: TGBTreePage; {parent Page}
  RightID: TGPageID; {Page ID of right sibling}
  right: TGBTreePage; {right sibling}
  right_extras: integer; {extra TBTreeItem in right sibling}
  leftID: TGPageID; {Page ID of left sibling}
  left: TGBTreePage; {left sibling}
  left_extras: integer; {extra TBTreeItem in left sibling}
begin
  parent := TGBtreePage.Create(SysRoot.PageCache, parentID);

  if position < parent.ItemCount then
  begin
    (*
     * There is a right sibling.
     * The position in the parent Page separating a Page from its
     *  right sibling is one greater than the position of the Page.
     *)
    position := position + 1;

    // work out and load the right sibling Page
    RightID := parent.Item[position].RightID;
    right := TGBtreePage.Create(SysRoot.PageCache, RightID);

    (*
     * work out how many TBTreeItem should be moved from the right
     *  sibling to the current Page to make them even
     *)
    right_extras := (right.ItemCount - HALF_FULL + 1) div 2;

    (*
     * regardless of whether TBTreeItem are borrowed from the right
     *  sibling, or the current Page is merged with the right
     *  sibling, the current Page has the separating Item from
     *  the parent Added to it.
     *)
    child := TGBtreePage.Create(SysRoot.PageCache, childID);
    child.ItemCount := child.ItemCount + 1;
    child.Item[child.ItemCount] := parent.Item[position];
    child.Item[child.ItemCount].RightID := right.LeftID;
    child.Free;
    right.Free;

    if right_extras > 0 then // borrow some TBTreeItem from the right sibling
      BalanceRight(position, childID, RightID, parentID, right_extras, shrunk)
    else
    begin // right sibling had nothing to spare
      if position <> 0 then
      begin
        // there is a left sibling, so try borrowing from it
        if position = 1 then
          leftID := parent.LeftID
        else
          leftID := parent.Item[position - 1].RightID;
        left := TGBtreePage.Create(SysRoot.PageCache, leftID);
        left_extras := (left.ItemCount - HALF_FULL + 1) div 2;
        left.Free;

        if left_extras > 0 then // extras can be borrowed from left sibling
          BalanceLeft(position, childID, leftID, parentID, left_extras, shrunk)
        else // left sibling had none to spare either, so give up and merge with the right sibling
          MergeRight(position, RightID, childID, parentID, shrunk);
      end
      else // there is no left sibling, so merge with the right sibling
        MergeRight(position, RightID, childID, parentID, shrunk);
    end;
  end
  else
  begin // there is no right sibling, so consider the left
    if position = 1 then
      leftID := parent.LeftID
    else
      leftID := parent.Item[position - 1].RightID;
    left := TGBtreePage.Create(SysRoot.PageCache, leftID);
    left_extras := (left.ItemCount - HALF_FULL + 1) div 2;
    left.Free;

    if left_extras > 0 then // left sibling has TBTreeItem to spare
      BalanceLeft(position, childID, leftID, parentID, left_extras, shrunk)
    else // left sibling has none to spare, so merge with the left sibling.
      MergeLeft(position, leftID, childID, parentID, shrunk);
  end;
  parent.Free;
end;

(*
 * Name:    FindPredecessor
 * Purpose: To find the leaf Item which is the predecessor of the Item
 *           to be Deleted (branch), swap the Item to be Deleted
 *           with its predecessor, and then Delete the Item.
 * Passed:  index:        the position in branch of Item
 *          descendID:   the Page ID of the descendant Page
 *          branchID:    the Page ID of the Page containing Item
 * Returns: shrunk:       True if descend is undersized
 *)
procedure TGBTreeIndex.FindPredecessor(index: integer;
  descendID: TGPageID; branchID: TGPageID;
  var shrunk: Boolean);
var
  childID: TGPageID; {right-most child of descend Page}
  descend: TGBTreePage; {current descendant Page}
  branch: TGBTreePage;
begin
  descend := TGBtreePage.Create(SysRoot.PageCache, descendID);

  // see if there is a right-most child
  childID := descend.Item[descend.ItemCount].RightID;
  if childID = 0 then
  begin
    (*
     * this is the end of the line. "descend" is the right-most
     *  descendant to the left of the Item being Deleted. Since
     *  this is its predecessor, the two TBTreeItem can be swapped
     *  without affecting the rest of the tree. Thus the
     *  descendant is copied into the ancestor, and the extra copy
     *  left of the descendant is then Deleted.
     *)
    branch := TGBtreePage.Create(SysRoot.PageCache, BranchID);
    descend.Item[descend.ItemCount].RightID := branch.Item[index].RightID;
    branch.Item[index] := descend.Item[descend.ItemCount];
    descend.ItemCount := descend.ItemCount - 1;
    shrunk := descend.ItemCount < HALF_FULL;

    // update the modified Pages
    branch.Free;
    descend.Free;
  end
  else
  begin // more descendants, so keep following them down
    FindPredecessor(index, childID, branchID, shrunk);

    // on the way back up, keep on topping up undersized TBTreePage as necessary.
    if shrunk then
      UnderFlow(descendID, childID, descend.ItemCount, shrunk);
  end;
end;

(*
 * Name:    Delete
 * Purpose: To Delete an Item with a given key from the current sub-tree.
 * Passed:  aKeyID:      the key to be Deleted
 *          branchID:    the Page ID of root of the current sub-tree
 * Returns: shrunk:      True if branch is undersized
 *)
function TGBTreeIndex.DeleteItem(aKeyID: TGPrimaryKey; branchID: TGPageID; var shrunk: Boolean): Boolean;
var
  branch: TGBTreePage; {the root of the current sub-tree}
  index: integer; {the position in branch of KEY}
  childID: TGPageID; {Page ID of child}
begin
  Result := False;
  if branchID = 0 then
  begin
    // Reached the bottom of the tree without finding the key to Delete
    shrunk := FALSE;
  end
  else
  begin // load in the current Page
    branch := TGBtreePage.Create(SysRoot.PageCache, branchID);

    if aKeyID > branch.Item[branch.ItemCount].KeyID then
    begin
      // aKeyID is to the right of the right-most Item, so follow the right-most line of descent
      index := branch.ItemCount;
      childID := branch.Item[branch.ItemCount].RightID;
      Result := DeleteItem(aKeyID, childID, shrunk);
      if shrunk then
        UnderFlow(branchID, childID, index, shrunk);
    end
    else
    begin // Locate the correct path to descend to the Item to be Deleted
      if FindItem(aKeyID, branch, index) then
      begin
        // KEY has been Located! Now see if it was on a leaf Page or not.
        if index = 1 then
          childID := branch.LeftID
        else
          childID := branch.Item[index - 1].RightID;

        if childID = 0 then
        begin
          (*
           * on a leaf Page already. Delete the Item, and
           *  set the "shrunk" flag if necessary so that
           *  the next level up can fix the undersize
           *  problem
           *)
          branch.ItemCount := branch.ItemCount - 1;
          shrunk := branch.ItemCount < HALF_FULL;
          Move(branch[index + 1]^, branch[index]^, SizeOf(TBTreeItem) * (1 + branch.ItemCount - index));
          branch.Free;
          FHeader.TotalItems := FHeader.TotalItems - 1;
          Modified := True;
        end
        else
        begin
          (*
           * not on a leaf node. Deletions can only be
           *  done directly on leaf nodes, so I must
           *  swap the current Item with its predecessor
           *)
          FindPredecessor(index, childID, branchID, shrunk);
          if shrunk then
            UnderFlow(branchID, childID, index - 1, shrunk);
        end;
      end
      else
      begin // not on this Page, so keep on descending and searching for it.
        if index = 1 then
          childID := branch.LeftID
        else
          childID := branch.Item[index - 1].RightID;
        DeleteItem(aKeyID, childID, shrunk);
        if shrunk then
          UnderFlow(branchID, childID, index - 1, shrunk);
      end;
      Result := True;
    end;
  end;
end;

(*
 * Name:    Delete_KEY
 * Purpose: To Delete a KEYfrom the library database.
 * Passed:
 *)
function TGBTreeIndex.Delete(aKeyID: TGPrimaryKey): Boolean;
var
  root: TGBTreePage; {the root Page of the B-tree}
  shrunk: Boolean; {is the root undersized?}
  old_root: TGPageID; {Page no. of root with no TBTreeItem}
begin
  // find out which KEY is to be Deleted
  DeleteItem(aKeyID, FHeader.RootNodeID, shrunk);
  Result := shrunk;
  if shrunk then
  begin
    root := TGBtreePage.Create(SysRoot.PageCache, FHeader.RootNodeID);
    if root.ItemCount = 0 then
    begin
      (*
       * The old root has shrunk to the point where it has no
       *  Items and only one left-most child. This left-most
       *  child must become the new root.
       * The old root can then be freed.
       *)
      old_root := FHeader.RootNodeID;
      FHeader.RootNodeID := root.LeftID;
      Modified := true;
      DeletePage(old_root);
    end
    else
      root.Free;
  end;
end;

{-------------------------------------------------------------------------------
 * LocateItem
 * Locate an Item with a given key in the current sub-tree.
 * Passed:  KEY:         the key to be located
 *          branchID:    the Page ID of root of the current sub-tree
 * Returns: PageID:      the ID of the Page containing KEY
 *          index:       the position within Page of KEY
-------------------------------------------------------------------------------}
function TGBTreeIndex.LocateItem(aKeyID: TGPrimaryKey; branchID: TGPageID; var index: integer): TGPageID;
var
  branch: TGBTreePage; {the root of the current sub-tree}
  i: integer; {index while searching}
begin
  Result := 0;

  // if branchID = 0 then we reached the bottom of the tree without finding the KEY

  if branchID <> 0 then // search the current Page...
  begin
    branch := TGBtreePage.Create(SysRoot.PageCache, branchID);

    if aKeyID > branch.Item[branch.ItemCount].KeyID then
    begin // search to right of right-most Item
      Result := LocateItem(aKeyID, branch.Item[branch.ItemCount].RightID, index);
    end
    else
    begin // find correct sub-tree to search
      if FindItem(aKeyID, branch, i) then
      begin // got it! Now just set up the return parameters
        Result := branchID;
        index := i;
      end
      else
      begin // follow appropriate sub-tree
        if i = 1 then
          Result := LocateItem(aKeyID, branch.LeftID, index)
        else
          Result := LocateItem(aKeyID, branch.Item[i - 1].RightID, index);
      end;
    end;
    branch.Free;
  end;
end;

{-------------------------------------------------------------------------
  TGBTreeIndex.Find_Key
-------------------------------------------------------------------------}
{**
  @Param aKeyID: TGPrimaryKey; var Item: TBTreeItem
  @Result Boolean
}
function TGBTreeIndex.FindKey(aKeyID: TGPrimaryKey; var Item: TBTreeItem): Boolean;
var
  PageID: TGPageID; {Page ID KEY occurred on, if any}
  Page: TGBTreePage; {Page KEY occurred on}
  index: integer; {position if KEY within Page}
begin
  Result := FALSE;
  PageID := LocateItem(aKeyID, FHeader.RootNodeID, index);
  if PageID <> 0 then
  begin
    Result := True;
    Page := TGBtreePage.Create(SysRoot.PageCache, PageID);
    Item := Page[index]^;
    Page.Free;
  end;
end;

function TGBTreeIndex.Find(aKeyID: TGPrimaryKey): TGPrimaryKey;
var
  Item: TBTreeItem;
begin
  if FindKey( aKeyID, item ) then
    Result := item.DataRowID
  else
    Result := 0;
end;

procedure TGBTreeIndex.Clear;
begin
  DeleteNodes(FHeader.RootNodeID);
  FHeader.PageCount := 0;
  FHeader.TreeDepth := 1;
  FHeader.RootNodeID := NewNode;
  Modified := True;
end;

procedure TGBTreeIndex.DeleteNodes(nodeID: TGPageID);
var
  node : TGBTreePage;
  idx : integer;
begin
  if nodeID = 0 then
    Exit;

  node := TGBTreePage.Create( SysRoot.PageCache, nodeID );

  // delete all the sub nodes
  for idx := 1 to node.ItemCount do
    DeleteNodes( node[idx].RightID );

  node.Free;
  SysRoot.PageCache.DeletePage(nodeID);
  Dec( FHeader.PageCount );
  Modified := True;
end;

{ TGBTreePage }

constructor TGBTreePage.Create(pageCache: TGPageCache; pageID: TGPageID);
begin
  inherited;
  FPgHead := PTBTreePageHeader( DataPtr );
end;

function TGBTreePage.GetItem(index: integer): PTBTreeItem;
begin
  Dec(index);
  Result := PTBTreeItem(DataPtr + SizeOf(TBTreePageHeader) + SizeOf( TBTreeItem ) * index );
end;

function TGBTreePage.GetItemCount: integer;
begin
  Result := FPgHead.ItemCount;
end;

function TGBTreePage.GetLeftID: TGRowID;
begin
  Result := FPgHead.LeftID;
end;

procedure TGBTreePage.SetItem(index: integer; const Value: PTBTreeItem);
begin
  GetItem(index)^ := Value^;
  Modified := true;
end;


procedure TGBTreePage.SetItemCount(const Value: integer);
begin
  FPgHead.ItemCount := Value;
  Modified := true;
end;

procedure TGBTreePage.SetLeftID(const Value: TGRowID);
begin
  FPgHead.LeftID := Value;
  Modified := true;
end;

procedure TGBTreeIndex.InternalLoad(aStream: TMemoryStream);
begin
  aStream.Read( FHeader, SizeOf( TGBTreeHeader ));
  Assert( FHeader.Magic = BTREE_MAGIC, 'BTree Magic Failed' );
end;

procedure TGBTreeIndex.InternalSave(aStream: TMemoryStream);
begin
  // Create a root page if this index is empty
  if FHeader.RootNodeID = 0 then
  begin
    FHeader.RootNodeID := NewNode;
    Modified := True;
  end;

  FHeader.Magic := BTREE_MAGIC;
  aStream.Write( FHeader, SizeOf( TGBTreeHeader ));
end;

procedure TGBTreeIndex.SaveToXML(el: TGXML_Element);
begin
  inherited;

  el.AddAttribute( 'PageCount', FHeader.PageCount, -1 );
  el.AddAttribute( 'Depth', FHeader.TreeDepth, -1 );
  el.AddAttribute( 'ItemCount', FHeader.TotalItems, -1 );
end;

initialization
  RegisterSDBClass( TGBTreeIndex, '' );
end.

