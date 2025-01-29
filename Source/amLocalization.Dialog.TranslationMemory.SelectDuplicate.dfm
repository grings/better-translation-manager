inherited FormSelectDuplicate: TFormSelectDuplicate
  ActiveControl = ComboBoxAction
  Caption = 'Duplicate translations found'
  ClientHeight = 355
  ClientWidth = 502
  ExplicitWidth = 518
  ExplicitHeight = 394
  TextHeight = 15
  inherited LayoutControlButtons: TdxLayoutControl
    Top = 300
    Width = 502
    ExplicitTop = 300
    ExplicitWidth = 502
    inherited ButtonOK: TcxButton
      Left = 233
      ExplicitLeft = 233
    end
    inherited ButtonCancel: TcxButton
      Left = 414
      TabOrder = 2
      ExplicitLeft = 414
    end
    object ButtonSkip: TcxButton [2]
      Left = 315
      Top = 13
      Width = 75
      Height = 25
      Action = ActionSkip
      TabOrder = 1
    end
    inherited LayoutItemButtonCancel: TdxLayoutItem
      Index = 3
    end
    inherited LayoutGroupButtons: TdxLayoutGroup
      ItemIndex = 2
    end
    object LayoutItemButtonSkip: TdxLayoutItem
      Parent = LayoutGroupButtons
      AlignHorz = ahRight
      CaptionOptions.Visible = False
      Control = ButtonSkip
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutEmptySpaceItem1: TdxLayoutEmptySpaceItem
      Parent = LayoutGroupButtons
      AlignHorz = ahRight
      SizeOptions.Height = 10
      SizeOptions.Width = 10
      CaptionOptions.Text = 'Empty Space Item'
      Index = 2
    end
  end
  inherited LayoutControlHeader: TdxLayoutControl
    Width = 502
    ExplicitWidth = 502
    inherited LayoutItemHeader: TdxLayoutLabeledItem
      CaptionOptions.Text = 'Select the translation to use'
    end
  end
  inherited LayoutControl: TdxLayoutControl
    Width = 502
    Height = 255
    ExplicitWidth = 502
    ExplicitHeight = 255
    object LabelSourceValueUnused: TcxLabel [0]
      Left = 79
      Top = 80
      AutoSize = False
      Properties.ShowAccelChar = False
      Properties.ShowEndEllipsis = True
      TabOrder = 3
      Transparent = True
      Height = 16
      Width = 0
    end
    object ListViewDuplicates: TcxListView [1]
      Left = 79
      Top = 103
      Width = 416
      Height = 118
      ColumnClick = False
      Columns = <
        item
          AutoSize = True
        end>
      HideSelection = False
      ReadOnly = True
      RowSelect = True
      ShowColumnHeaders = False
      TabOrder = 4
      ViewStyle = vsReport
      OnDblClick = ListViewDuplicatesDblClick
    end
    object CheckBoxAll: TcxCheckBox [2]
      Left = 295
      Top = 7
      Caption = '&Do this for all ambiguities'
      Enabled = False
      Properties.OnChange = CheckBoxAllPropertiesChange
      Style.HotTrack = False
      TabOrder = 1
      Transparent = True
    end
    object ComboBoxAction: TcxComboBox [3]
      Left = 79
      Top = 7
      Properties.DropDownListStyle = lsFixedList
      Properties.Items.Strings = (
        'Use the translation I select'
        'Use first available translation'
        'Skip value')
      Properties.OnChange = ComboBoxActionPropertiesChange
      Style.HotTrack = False
      TabOrder = 0
      Text = 'Use the translation I select'
      Width = 192
    end
    object LabelContextUnused: TcxLabel [4]
      Left = 79
      Top = 54
      AutoSize = False
      Properties.ShowAccelChar = False
      Properties.ShowEndEllipsis = True
      TabOrder = 2
      Transparent = True
      Height = 19
      Width = 0
    end
    object CheckBoxApplyToIdentical: TcxCheckBox [5]
      Left = 79
      Top = 228
      Caption = '&Use this translation for identical ambiguities'
      Style.HotTrack = False
      TabOrder = 5
      Transparent = True
    end
    inherited LayoutControlGroup_Root: TdxLayoutGroup
      AlignVert = avClient
      ItemIndex = 2
    end
    object LayoutItemSource: TdxLayoutItem
      Parent = dxLayoutGroup3
      CaptionOptions.Text = 'Source text:'
      Control = LabelSourceValueUnused
      ControlOptions.OriginalHeight = 16
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object LayoutItemTranslationList: TdxLayoutItem
      Parent = LayoutControlGroup_Root
      AlignVert = avClient
      CaptionOptions.AlignVert = tavTop
      CaptionOptions.Text = '&Translations:'
      Control = ListViewDuplicates
      ControlOptions.OriginalHeight = 156
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 4
    end
    object dxLayoutItem5: TdxLayoutItem
      Parent = dxLayoutGroup2
      CaptionOptions.Text = 'cxCheckBox1'
      CaptionOptions.Visible = False
      Control = CheckBoxAll
      ControlOptions.OriginalHeight = 20
      ControlOptions.OriginalWidth = 158
      ControlOptions.ShowBorder = False
      Enabled = False
      Index = 2
    end
    object dxLayoutItem6: TdxLayoutItem
      Parent = dxLayoutGroup2
      CaptionOptions.Text = '&Action:'
      Control = ComboBoxAction
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 192
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutEmptySpaceItem2: TdxLayoutEmptySpaceItem
      Parent = LayoutControlGroup_Root
      SizeOptions.Height = 10
      SizeOptions.Width = 10
      CaptionOptions.Text = 'Empty Space Item'
      Index = 1
    end
    object dxLayoutGroup2: TdxLayoutGroup
      Parent = LayoutControlGroup_Root
      CaptionOptions.Visible = False
      ItemIndex = 1
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 0
    end
    object dxLayoutEmptySpaceItem3: TdxLayoutEmptySpaceItem
      Parent = dxLayoutGroup2
      SizeOptions.Height = 10
      SizeOptions.Width = 10
      CaptionOptions.Text = 'Empty Space Item'
      Index = 1
    end
    object LayoutItemContext: TdxLayoutItem
      Parent = dxLayoutGroup1
      CaptionOptions.Text = 'Context:'
      Control = LabelContextUnused
      ControlOptions.OriginalHeight = 19
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem8: TdxLayoutItem
      Parent = LayoutControlGroup_Root
      CaptionOptions.Text = ' '
      Control = CheckBoxApplyToIdentical
      ControlOptions.OriginalHeight = 20
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 5
    end
    object LayoutItemContextValue: TdxLayoutLabeledItem
      Parent = dxLayoutGroup1
      AlignHorz = ahClient
      CaptionOptions.EllipsisPosition = epPathEllipsis
      CaptionOptions.ShowAccelChar = False
      CaptionOptions.Text = '-'
      Index = 1
    end
    object dxLayoutGroup1: TdxLayoutGroup
      Parent = LayoutControlGroup_Root
      CaptionOptions.Text = 'New Group'
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 2
    end
    object LayoutItemSourceValue: TdxLayoutLabeledItem
      Parent = dxLayoutGroup3
      AlignHorz = ahClient
      CaptionOptions.EllipsisPosition = epEndEllipsis
      CaptionOptions.ShowAccelChar = False
      CaptionOptions.Text = '-'
      Index = 1
    end
    object dxLayoutGroup3: TdxLayoutGroup
      Parent = LayoutControlGroup_Root
      CaptionOptions.Text = 'New Group'
      ItemIndex = 1
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 3
    end
  end
  inherited ActionList: TActionList
    inherited ActionOK: TAction
      OnUpdate = ActionOKUpdate
    end
    object ActionSkip: TAction
      Caption = 'Skip'
      Hint = 'Skip this value'
      OnExecute = ActionSkipExecute
    end
  end
end
