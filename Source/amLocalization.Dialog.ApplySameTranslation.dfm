inherited FormApplySameTranslation: TFormApplySameTranslation
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSizeable
  Caption = 'Apply translation'
  ClientHeight = 441
  ClientWidth = 784
  ExplicitWidth = 800
  TextHeight = 15
  inherited LayoutControlButtons: TdxLayoutControl
    Top = 386
    Width = 784
    ExplicitTop = 386
    ExplicitWidth = 784
    inherited ButtonOK: TcxButton
      Left = 515
      TabOrder = 1
      ExplicitLeft = 515
    end
    inherited ButtonCancel: TcxButton
      Left = 696
      TabOrder = 3
      ExplicitLeft = 696
    end
    object ComboBoxNoPromptAction: TcxComboBox [2]
      Left = 200
      Top = 15
      Properties.DropDownListStyle = lsFixedList
      Properties.Items.Strings = (
        'Do not apply translations'
        'Apply translations'
        'Prompt')
      Properties.OnEditValueChanged = ComboBoxNoPromptActionPropertiesEditValueChanged
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 0
      Text = 'Prompt'
      Width = 146
    end
    object ButtonSkip: TcxButton [3]
      Left = 597
      Top = 13
      Width = 75
      Height = 25
      Action = ActionSkip
      ModalResult = 5
      TabOrder = 2
    end
    inherited LayoutItemButtonOK: TdxLayoutItem
      Index = 1
    end
    inherited LayoutItemButtonCancel: TdxLayoutItem
      Index = 4
    end
    inherited LayoutGroupButtons: TdxLayoutGroup
      ItemIndex = 3
    end
    object LayoutItemRemaining: TdxLayoutItem
      Parent = LayoutGroupButtons
      AlignVert = avCenter
      CaptionOptions.Text = 'For the remaining %d translations:'
      Control = ComboBoxNoPromptAction
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 146
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object LayoutItemButtonSkip: TdxLayoutItem
      Parent = LayoutGroupButtons
      AlignHorz = ahRight
      CaptionOptions.Visible = False
      Control = ButtonSkip
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutEmptySpaceItem1: TdxLayoutEmptySpaceItem
      Parent = LayoutGroupButtons
      AlignHorz = ahRight
      SizeOptions.Height = 10
      SizeOptions.Width = 10
      CaptionOptions.Text = 'Empty Space Item'
      Index = 3
    end
  end
  inherited LayoutControlHeader: TdxLayoutControl
    Width = 784
    Visible = True
    ExplicitWidth = 784
    inherited LayoutItemHeader: TdxLayoutLabeledItem
      CaptionOptions.Text = 
        'The translation can be applied to other properties with the same' +
        ' or similar values (%d of %d)'
    end
  end
  inherited LayoutControl: TdxLayoutControl
    Width = 784
    Height = 341
    ExplicitWidth = 784
    ExplicitHeight = 341
    object Grid: TcxGrid [0]
      Left = 7
      Top = 30
      Width = 770
      Height = 304
      TabOrder = 0
      object GridView: TcxGridTableView
        OnKeyPress = GridViewKeyPress
        Navigator.Buttons.CustomButtons = <>
        ScrollbarAnnotations.CustomAnnotations = <>
        OnCanSelectRecord = GridViewCanSelectRecord
        OnCellDblClick = GridViewCellDblClick
        OnEditing = GridViewEditing
        OnSelectionChanged = GridViewSelectionChanged
        DataController.Options = [dcoAssignGroupingValues, dcoAssignMasterDetailKeys, dcoSaveExpanding, dcoImmediatePost]
        DataController.Summary.DefaultGroupSummaryItems = <>
        DataController.Summary.FooterSummaryItems = <>
        DataController.Summary.SummaryGroups = <>
        FixedDataRows.PinClickAction = rpcaNone
        FixedDataRows.PinVisibility = rpvAlways
        OptionsBehavior.CellHints = True
        OptionsData.Deleting = False
        OptionsData.Inserting = False
        OptionsSelection.MultiSelect = True
        OptionsSelection.CheckBoxPosition = cbpIndicator
        OptionsSelection.CheckBoxVisibility = [cbvDataRow, cbvColumnHeader]
        OptionsSelection.MultiSelectMode = msmPersistent
        OptionsView.CellEndEllipsis = True
        OptionsView.ColumnAutoWidth = True
        OptionsView.GroupByBox = False
        OptionsView.GroupByHeaderLayout = ghlHorizontal
        OptionsView.Indicator = True
        Styles.OnGetContentStyle = GridViewStylesGetContentStyle
        Styles.OnGetInactiveStyle = GridViewStylesGetInactiveStyle
        Styles.OnGetSelectionStyle = GridViewStylesGetSelectionStyle
        OnCustomDrawIndicatorCell = GridViewCustomDrawIndicatorCell
        object GridViewColumnModule: TcxGridColumn
          Caption = 'Module'
          Options.Editing = False
          Width = 108
        end
        object GridViewColumnElement: TcxGridColumn
          Caption = 'Element'
          Options.Editing = False
          Width = 118
        end
        object GridViewColumnProperty: TcxGridColumn
          Caption = 'Property'
          Options.Editing = False
          Width = 122
        end
        object GridViewColumnValue: TcxGridColumn
          Caption = 'Value'
          Options.Editing = False
          Width = 88
        end
        object GridViewColumnOld: TcxGridColumn
          Caption = 'Old translation'
          Options.Editing = False
          Width = 152
        end
        object GridViewColumnNew: TcxGridColumn
          Caption = 'New translation'
          PropertiesClassName = 'TcxTextEditProperties'
          Properties.OnEditValueChanged = GridViewColumnNewPropertiesEditValueChanged
          Width = 148
        end
      end
      object GridLevel: TcxGridLevel
        GridView = GridView
      end
    end
    inherited LayoutControlGroup_Root: TdxLayoutGroup
      AlignHorz = ahParentManaged
      AlignVert = avParentManaged
    end
    object LayoutItemGrid: TdxLayoutItem
      Parent = LayoutControlGroup_Root
      AlignVert = avClient
      CaptionOptions.Visible = False
      Control = Grid
      ControlOptions.OriginalHeight = 200
      ControlOptions.OriginalWidth = 250
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutLabeledItem1: TdxLayoutLabeledItem
      Parent = LayoutControlGroup_Root
      CaptionOptions.Text = 
        'Select the properties below that you would like to apply the tra' +
        'nslation to.'
      Index = 0
    end
  end
  inherited ActionList: TActionList
    inherited ActionOK: TAction
      Caption = 'Apply'
    end
    inherited ActionCancel: TAction
      Caption = 'Abort'
    end
    object ActionSkip: TAction
      Caption = 'Skip'
      OnExecute = ActionEmptyExecute
    end
  end
end
