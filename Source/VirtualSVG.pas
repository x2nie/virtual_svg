unit VirtualSVG;

interface

uses
  SysUtils,
  GR32;

const
  VSVGVersion = '0.0.1';

{$if CompilerVersion < 20}
type
  UnicodeString = WideString;
  RawByteString = AnsiString;
  PByte = PAnsiChar;
{$ifend}

type
  PVirtualNode = ^TVirtualTag;

  // Be careful when adding new states as this might change the size of the type which in turn
  // changes the alignment in the node record as well as the stream chunks.
  // Do not reorder the states and always add new states at the end of this enumeration in order to avoid
  // breaking existing code.
  TVirtualTagState = (
    vsInitialized,       // Set after the node has been initialized.
    vsChecking,          // Node's check state is changing, avoid propagation.
    vsCutOrCopy,         // Node is selected as cut or copy and paste source.
    vsDisabled,          // Set if node is disabled.
    vsDeleting,          // Set when the node is about to be freed.
    vsExpanded,          // Set if the node is expanded.
    vsHasChildren,       // Indicates the presence of child nodes without actually setting them.
    vsVisible,           // Indicate whether the node is visible or not (independant of the expand states of its parents).
    vsSelected,          // Set if the node is in the current selection.
    vsOnFreeNodeCallRequired,   // Set if user data has been set which requires OnFreeNode.
    vsAllChildrenHidden, // Set if vsHasChildren is set and no child node has the vsVisible flag set.
    vsClearing,          // A node's children are being deleted. Don't register structure change event.
    vsMultiline,         // Node text is wrapped at the cell boundaries instead of being shorted.
    vsHeightMeasured,    // Node height has been determined and does not need a recalculation.
    vsToggling,          // Set when a node is expanded/collapsed to prevent recursive calls.
    vsFiltered           // Indicates that the node should not be painted (without effecting its children).
  );
  TVirtualTagStates = set of TVirtualTagState;

  TChangeReason = (
    crIgnore,       // used as placeholder
    crAccumulated,  // used for delayed changes
    crChildAdded,   // one or more child nodes have been added
    crChildDeleted, // one or more child nodes have been deleted
    crNodeAdded,    // a node has been added
    crNodeCopied,   // a node has been duplicated
    crNodeMoved     // a node has been moved to a new place
  ); // desribes what made a structure change event happen


  // Abstract Node
  TSvgNode = class(TObject)
  end;

  // Uniforming
  TSvgNodeClass = class of TSvgNode;


  TSvgSvg = class(TSvgNode)
  end;

  TSvgRectangle = class(TSvgNode)
  end;

  TDataRectangle = record
    x,y,
    rx,ry,
    width, height : TFloat;
  end;


  TVirtualTag = packed record
    Index,                   // index of node with regard to its parent
    ChildCount: Cardinal;    // number of child nodes
    //NodeHeight: Word;        // height in pixels
    States: TVirtualTagStates; // states describing various properties of the node (expanded, initialized etc.)
    NodeClass : TSvgNodeClass; // class that best used for modifying this record
    //Align: Byte;             // line/button alignment
    //CheckState: TCheckState; // indicates the current check state (e.g. checked, pressed etc.)
    //CheckType: TCheckType;   // indicates which check type shall be used for this node
    //Dummy: Byte;             // dummy value to fill DWORD boundary
    TotalCount,              // sum of this node, all of its child nodes and their child nodes etc.
    TotalHeight: Cardinal;   // height in pixels this node covers on screen including the height of all of its
                             // children
    // Note: Some copy routines require that all pointers (as well as the data area) in a node are
    //       located at the end of the node! Hence if you want to add new member fields (except pointers to internal
    //       data) then put them before field Parent.
    Parent,                  // reference to the node's parent (for the root this contains the treeview)
    PrevSibling,             // link to the node's previous sibling or nil if it is the first node
    NextSibling,             // link to the node's next sibling or nil if it is the last node
    FirstChild,              // link to the node's first child...
    LastChild: PVirtualNode; // link to the node's last child...
    Data: record end;        // this is a placeholder, each node gets extra data determined by NodeDataSize
  end;

  //TVirtualTagClass = type TVirtualTag;


  TSVG = class(TNotifiablePersistent)
  private
    FRoot: PVirtualNode;
    //FNodeDataSize,
    FTotalInternalDataSize : Cardinal;
    procedure AdjustTotalCount(Node: PVirtualNode; Value: Integer; relative: Boolean = False);    
    procedure InitRootTag(OldSize: Cardinal = 0);
    function MakeNewTag(UserDataSize : Integer = 0): PVirtualNode;
    procedure SetChildCount(Node: PVirtualNode; NewChildCount: Cardinal; UserDataSize : Integer = 0);
  protected
    procedure DoFreeNode(Node: PVirtualNode); virtual;
    procedure StructureChange(Node: PVirtualNode; Reason: TChangeReason); virtual;  
  public
    function AddChild(Parent: PVirtualNode; UserDataSize : Integer = 0;
      UserData: Pointer = nil; CopyData:Boolean = False): PVirtualNode; overload; virtual;
    function AddChild(Parent: PVirtualNode; UserDataSize : Integer; const UserData): PVirtualNode; overload; virtual;
    
    procedure AfterConstruction; override;
    procedure DeleteChildren(Node: PVirtualNode; ResetHasChildren: Boolean = False);
    procedure DeleteNode(Node: PVirtualNode; Reindex: Boolean = True);
    function GetData(Node: PVirtualNode): Pointer;

    property RootTag: PVirtualNode read FRoot;
  end;

const
  tagSVG  =  1;
  tagContainer  = 2;




implementation

const
  TreeTagSize = (SizeOf(TVirtualTag) + (SizeOf(Pointer) - 1)) and not (SizeOf(Pointer) - 1); // used for node allocation and access to internal data

procedure ZeroMemory(Destination: Pointer; Length: Cardinal);
begin
  FillChar(Destination^, Length, 0);
end;


function NewRectangle(): PVirtualNode ;
begin

end;



{ TSVG }


function TSVG.AddChild(Parent: PVirtualNode; UserDataSize : Integer;
  UserData: Pointer; CopyData:Boolean): PVirtualNode;

// Adds a new node to the given parent node. This is simply done by increasing the child count of the
// parent node. If Parent is nil then the new node is added as (last) top level node.
// UserData can be used to set the first sizeof(Pointer) bytes of the user data area to an initial value which can be used
// in OnInitNode and will also cause to trigger the OnFreeNode event (if <> nil) even if the node is not yet
// "officially" initialized.
// AddChild is a compatibility method and will implicitly validate the parent node. This is however
// against the virtual paradigm and hence I dissuade from its usage.

var
  NodeData: ^Pointer;

begin
  //if not (toReadOnly in FOptions.FMiscOptions) then
  begin
    //CancelEditNode;

    if Parent = nil then
      Parent := FRoot;
    //if not (vsInitialized in Parent.States) then
      //InitNode(Parent);

    // Locally stop updates of the tree in order to avoid usage of the new node before it is correctly set up.
    // If the update count was 0 on enter then there will be a correct update at the end of this method.
    BeginUpdate;
    try
      SetChildCount(Parent, Parent.ChildCount + 1, UserDataSize);
      // Update the hidden children flag of the parent. Nodes are added as being visible by default.
      Exclude(Parent.States, vsAllChildrenHidden);
    finally
      EndUpdate;
    end;
    Result := Parent.LastChild;

    // Check if there is initial user data and there is also enough user data space allocated.
    if Assigned(UserData) then
      //if FNodeDataSize >= SizeOf(Pointer) then
      if UserDataSize >= SizeOf(Pointer) then
      begin
        NodeData := Pointer(PByte(@Result.Data) + FTotalInternalDataSize);
        if CopyData then
          Move(UserData^,NodeData^, UserDataSize)
        else
          NodeData^ := UserData;
        Include(Result.States, vsOnFreeNodeCallRequired);
      end
      else
        raise Exception.Create('CannotSetUserData');
        //ShowError(SCannotSetUserData, hcTFCannotSetUserData);

    {InvalidateCache;
    if FUpdateCount = 0 then
    begin
      ValidateCache;
      if tsStructureChangePending in FStates then
      begin
        if Parent = FRoot then
          StructureChange(nil, crChildAdded)
        else
          StructureChange(Parent, crChildAdded);
      end;

      if (toAutoSort in FOptions.FAutoOptions) and (FHeader.FSortColumn > InvalidColumn) then
        Sort(Parent, FHeader.FSortColumn, FHeader.FSortDirection, True);

      InvalidateToBottom(Parent);
      UpdateScrollbars(True);
    end;}
  end
  //else
    //Result := nil;

end;

function TSVG.AddChild(Parent: PVirtualNode; UserDataSize : Integer; const UserData): PVirtualNode;
begin
  Result := AddChild(Parent, UserDataSize, @UserData, True);
end;

procedure TSVG.AdjustTotalCount(Node: PVirtualNode; Value: Integer;
  relative: Boolean);

// Sets a node's total count to the given value and recursively adjusts the parent's total count
// (actually, the adjustment is done iteratively to avoid function call overheads).

var
  Difference: Integer;
  Run: PVirtualNode;

begin
  if relative then
    Difference := Value
  else
    Difference := Value - Integer(Node.TotalCount);
  if Difference <> 0 then
  begin
    Run := Node;
    // Root node has as parent the tree view.
    while Assigned(Run) and (Run <> Pointer(Self)) do
    begin
      Inc(Integer(Run.TotalCount), Difference);
      Run := Run.Parent;
    end;
  end;
end;

procedure TSVG.AfterConstruction;
begin
  inherited;

  if FRoot = nil then
    InitRootTag;
end;

procedure TSVG.DeleteChildren(Node: PVirtualNode;
  ResetHasChildren: Boolean);

// Removes all children and their children from memory without changing the vsHasChildren style by default.

var
  Run,
  Mark: PVirtualNode;
  LastTop,
  LastLeft,
  NewSize: Integer;
  ParentVisible: Boolean;

begin
  if Assigned(Node) and (Node.ChildCount > 0) {and not (toReadOnly in FOptions.FMiscOptions)} then
  begin
    //Assert(not (tsIterating in FStates), 'Deleting nodes during tree iteration leads to invalid pointers.');

    // The code below uses some flags for speed improvements which may cause invalid pointers if updates of
    // the tree happen. Hence switch updates off until we have finished the operation.
    BeginUpdate;
    try
      //InterruptValidation;
      //LastLeft := -FEffectiveOffsetX;
      //LastTop := FOffsetY;

      // Make a local copy of the visibility state of this node to speed up
      // adjusting the visible nodes count.
      ParentVisible := Node = FRoot;
      //if not ParentVisible then
        //ParentVisible := FullyVisible[Node] and (vsExpanded in Node.States);

      // Show that we are clearing the child list, to avoid registering structure change events.
      Include(Node.States, vsClearing);
      Run := Node.LastChild;
      while Assigned(Run) do
      begin
        //if ParentVisible and IsEffectivelyVisible[Run] then
          //Dec(FVisibleCount);

        Include(Run.States, vsDeleting);
        Mark := Run;
        Run := Run.PrevSibling;
        // Important, to avoid exchange of invalid pointers while disconnecting the node.
        if Assigned(Run) then
          Run.NextSibling := nil;
        DeleteNode(Mark);
      end;
      Exclude(Node.States, vsClearing);
      if ResetHasChildren then
        Exclude(Node.States, vsHasChildren);
      if Node <> FRoot then
        Exclude(Node.States, vsExpanded);
      Node.ChildCount := 0;
      if (Node = FRoot) or (vsDeleting in Node.States) then
      begin
        //Node.TotalHeight := FDefaultNodeHeight + NodeHeight[Node];
        Node.TotalCount := 1;
      end
      else
      begin
        //AdjustTotalHeight(Node, NodeHeight[Node]);
        AdjustTotalCount(Node, 1);
      end;
      Node.FirstChild := nil;
      Node.LastChild := nil;
    finally
      EndUpdate;
    end;

    //InvalidateCache;
    {if UpdateCount = 0 then
    begin
      NewSize := PackArray(FSelection, FSelectionCount);
      if NewSize > -1 then
      begin
        FSelectionCount := NewSize;
        SetLength(FSelection, FSelectionCount);
      end;

      ValidateCache;
      UpdateScrollbars(True);
      // Invalidate entire tree if it scrolled e.g. to make the last node also the
      // bottom node in the treeview.
      if (LastLeft <> FOffsetX) or (LastTop <> FOffsetY) then
        Invalidate
      else
        InvalidateToBottom(Node);
    end;
    StructureChange(Node, crChildDeleted);}
  end
  else if ResetHasChildren then
    Exclude(Node.States, vsHasChildren);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSVG.DeleteNode(Node: PVirtualNode; Reindex: Boolean = True);

var
  LastTop,
  LastLeft: Integer;
  LastParent: PVirtualNode;
  WasInSynchMode: Boolean;
  ParentClearing: Boolean;

begin
  if Assigned(Node) and (Node <> FRoot) {and not (toReadOnly in FOptions.FMiscOptions)} then
  begin
    //Assert(not (tsIterating in FStates), 'Deleting nodes during tree iteration leads to invalid pointers.');

    // Determine parent node for structure change notification.
    ParentClearing := vsClearing in Node.Parent.States;
    LastParent := Node.Parent;

    if not ParentClearing then
    begin
      if LastParent = FRoot then
        StructureChange(nil, crChildDeleted)
      else
        StructureChange(LastParent, crChildDeleted);
    end;

    //LastLeft := -FEffectiveOffsetX;
    //LastTop := FOffsetY;

    {if vsSelected in Node.States then
    begin
      if UpdateCount = 0 then
      begin
        // Go temporarily into sync mode to avoid a delayed change event for the node
        // when unselecting.
        WasInSynchMode := tsSynchMode in FStates;
        Include(FStates, tsSynchMode);
        RemoveFromSelection(Node);
        if not WasInSynchMode then
          Exclude(FStates, tsSynchMode);
        InvalidateToBottom(LastParent);
      end
      else
        InternalRemoveFromSelection(Node);
    end
    else
      InvalidateToBottom(LastParent);

    if tsHint in FStates then
    begin
      Application.CancelHint;
      DoStateChange([], [tsHint]);
    end;

    if not ParentClearing then
      InterruptValidation;
    }
    DeleteChildren(Node);
    //InternalDisconnectNode(Node, False, Reindex);
    DoFreeNode(Node);

    {if not ParentClearing then
    begin
      DetermineHiddenChildrenFlag(LastParent);
      InvalidateCache;
      if FUpdateCount = 0 then
      begin
        ValidateCache;
        UpdateScrollbars(True);
        // Invalidate entire tree if it scrolled e.g. to make the last node also the
        // bottom node in the treeview.
        if (LastLeft <> FOffsetX) or (LastTop <> FOffsetY) then
          Invalidate;
      end;
    end;}
  end;
end;

//----------------------------------------------------------------------------------------------------------------------


procedure TSVG.DoFreeNode(Node: PVirtualNode);

begin
  {if Node = FLastChangedNode then
    FLastChangedNode := nil;
  if Node = FCurrentHotNode then
    FCurrentHotNode := nil;
  if Node = FDropTargetNode then
    FDropTargetNode := nil;
  if Node = FLastStructureChangeNode then
    FLastStructureChangeNode := nil;
  if Assigned(FOnFreeNode) and ([vsInitialized, vsOnFreeNodeCallRequired] * Node.States <> []) then
    FOnFreeNode(Self, Node);}
  FreeMem(Node);

end;

function TSVG.GetData(Node: PVirtualNode): Pointer;
// Returns the address of the user defined data area in the node.

begin
  //Assert(FNodeDataSize > 0, 'NodeDataSize not initialized.');
  if {(FNodeDataSize <= 0) or} (Node = nil) or (Node = FRoot) then
    Result := nil
  else begin
    Result := PByte(@Node.Data) + FTotalInternalDataSize;
    Include(Node.States, vsOnFreeNodeCallRequired); // We now need to call OnFreeNode, see bug #323
  end;
end;

procedure TSVG.InitRootTag(OldSize: Cardinal);

// Reinitializes the root node.

var
  NewSize: Cardinal;

begin
  NewSize := TreeTagSize + FTotalInternalDataSize;
  if FRoot = nil then
    FRoot := AllocMem(NewSize)
  else
  begin
    ReallocMem(FRoot, NewSize);
    ZeroMemory(PByte(FRoot) + OldSize,  NewSize - OldSize);
  end;

  with FRoot^ do
  begin
    // Indication that this node is the root node.
    PrevSibling := FRoot;
    NextSibling := FRoot;
    Parent := Pointer(Self);
    States := [vsInitialized, vsExpanded, vsHasChildren, vsVisible];
    //TotalHeight := FDefaultNodeHeight;
    TotalCount := 1;
    //TotalHeight := FDefaultNodeHeight;
    //NodeHeight := FDefaultNodeHeight;
    //Align := 50;
  end;
end;

function TSVG.MakeNewTag(UserDataSize : Integer = 0): PVirtualNode;

var
  Size: Cardinal;

begin
  Size := TreeTagSize;
  //if not (csDesigning in ComponentState) then
  begin
    // Make sure FNodeDataSize is valid.
    //if FTagDataSize = -1 then
      //ValidateNodeDataSize(FNodeDataSize);

    // Take record alignment into account.
    //Inc(Size, FTagDataSize);
    Inc(Size, UserDataSize);
  end;

  Result := AllocMem(Size + FTotalInternalDataSize);

  // Fill in some default values.
  with Result^ do
  begin
    TotalCount := 1;
    //TotalHeight := FDefaultNodeHeight;
    //NodeHeight := FDefaultNodeHeight;
    States := [vsVisible];
    //Align := 50;
  end;
end;

procedure TSVG.SetChildCount(Node: PVirtualNode; NewChildCount: Cardinal; UserDataSize : Integer = 0);
// Changes a node's child structure to accomodate the new child count. This is used to add or delete
// child nodes to/from the end of the node's child list. To insert or delete a specific node a separate
// routine is used.

var
  Remaining: Cardinal;
  Index: Cardinal;
  Child: PVirtualNode;
  Count: Integer;
  NewHeight: Integer;

begin
  //if not (toReadOnly in FOptions.FMiscOptions) then
  begin
    if Node = nil then
      Node := FRoot;

    if NewChildCount = 0 then
      DeleteChildren(Node)
    else
    begin
      // If nothing changed then do nothing.
      if NewChildCount <> Node.ChildCount then
      begin
        //InterruptValidation;
        NewHeight := 0;

        if NewChildCount > Node.ChildCount then
        begin
          Remaining := NewChildCount - Node.ChildCount;
          Count := Remaining;

          // New nodes to add.
          if Assigned(Node.LastChild) then
            Index := Node.LastChild.Index + 1
          else
          begin
            Index := 0;
            Include(Node.States, vsHasChildren);
          end;
          Node.States := Node.States - [vsAllChildrenHidden, vsHeightMeasured];

          // New nodes are by default always visible, so we don't need to check the visibility.
          while Remaining > 0 do
          begin
            Child := MakeNewTag(UserDataSize);
            Child.Index := Index;
            Child.PrevSibling := Node.LastChild;
            if Assigned(Node.LastChild) then
              Node.LastChild.NextSibling := Child;
            Child.Parent := Node;
            Node.LastChild := Child;
            if Node.FirstChild = nil then
              Node.FirstChild := Child;
            Dec(Remaining);
            Inc(Index);

            // The actual node height will later be computed once it is clear
            // whether this node has a variable node height or not.
            //Inc(NewHeight, Child.NodeHeight);
          end;

          {if vsExpanded in Node.States then
          begin
            AdjustTotalHeight(Node, NewHeight, True);
            if FullyVisible[Node] then
              Inc(Integer(FVisibleCount), Count);
          end;}

          AdjustTotalCount(Node, Count, True);
          Node.ChildCount := NewChildCount;
          {if (UpdateCount = 0) and (toAutoSort in FOptions.FAutoOptions) and (FHeader.FSortColumn > InvalidColumn) then
            Sort(Node, FHeader.FSortColumn, FHeader.FSortDirection, True);}

          //InvalidateCache;
        end
        else
        begin
          // Nodes have to be deleted.
          Remaining := Node.ChildCount - NewChildCount;
          while Remaining > 0 do
          begin
            DeleteNode(Node.LastChild);
            Dec(Remaining);
          end;
        end;

        {if UpdateCount = 0 then
        begin
          ValidateCache;
          UpdateScrollBars(True);
          Invalidate;
        end;}

        if Node = FRoot then
          StructureChange(nil, crChildAdded)
        else
          StructureChange(Node, crChildAdded);
      end;
    end;
  end;
end;

procedure TSVG.StructureChange(Node: PVirtualNode; Reason: TChangeReason);
begin

end;

end.
