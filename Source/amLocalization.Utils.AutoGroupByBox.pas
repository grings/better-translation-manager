unit amLocalization.Utils.AutoGroupByBox;

(*
 * Copyright © 2019 Anders Melander
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *)

interface

uses
  cxGridCustomView,
  cxGridTableView;

type
  //
  // Automatically display or hide a TcxGridTableView GroupByBox.
  //
  // - If the view is grouped, the box is made visible.
  //
  // - If the user drags a column header, the box is made visible.
  //
  // - When the user stops dragging, the box is kept visible if the view is
  //   now grouped or is hidden if it is not grouped.
  //
  // Note that the view must be a TcxGridTableView.
  //
  GroupByBoxAutoDisplay = record
    // Mouse up and down handlers.
    // Call these from the view's OnMouseMove and OnMouseUp event handlers.
    class procedure MouseMoveHandler(ASite: TcxGridSite); static;
    class procedure MouseUpHandler(ASite: TcxGridSite); static;

    // Show or hide the GroupByBox depending on the current grouping of the view.
    class procedure Update(ASite: TcxGridSite); overload; static;
    class procedure Update(AView: TcxGridTableView); overload; static;
  end;

implementation

{ GroupByBoxAutoDisplay }

class procedure GroupByBoxAutoDisplay.MouseMoveHandler(ASite: TcxGridSite);
begin
  var View := ASite.GridView as cxGridTableView.TcxGridTableView;

  // Is grouping allowed?
  if (not View.OptionsCustomize.ColumnGrouping) then
    exit;

  // Display the GroupByBox if an item is being dragged, and that item can be grouped, or
  // if the view is already grouped.
  // Otherwise hide the GroupByBox.
  var DisplayGroupByBox := ((View.Controller.MovingItem <> nil) and (TcxGridColumnOptions(View.Controller.MovingItem.Options).Grouping)) or
    (View.GroupedColumnCount > 0);

  // Are we hiding or displaying the the group box?
  if (View.OptionsView.GroupByBox = DisplayGroupByBox) then
    exit;

  View.OptionsView.GroupByBox := DisplayGroupByBox;

  if (DisplayGroupByBox) then
    ASite.MouseCapture := True;
end;

class procedure GroupByBoxAutoDisplay.MouseUpHandler(ASite: TcxGridSite);
begin
  Update(ASite);
end;

class procedure GroupByBoxAutoDisplay.Update(ASite: TcxGridSite);
begin
  var View := ASite.GridView as cxGridTableView.TcxGridTableView;
  Update(View);
end;

class procedure GroupByBoxAutoDisplay.Update(AView: TcxGridTableView);
begin
  // Is grouping allowed?
  if (not AView.OptionsCustomize.ColumnGrouping) then
    exit;

  // Only display group box if we are grouping on anything
  AView.OptionsView.GroupByBox := (AView.GroupedColumnCount > 0);
end;

end.
