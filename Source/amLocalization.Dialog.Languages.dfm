inherited FormLanguages: TFormLanguages
  Caption = 'Languages'
  ClientHeight = 371
  ClientWidth = 601
  ExplicitWidth = 617
  ExplicitHeight = 410
  TextHeight = 15
  inherited LayoutControlButtons: TdxLayoutControl
    Top = 316
    Width = 601
    ExplicitTop = 316
    ExplicitWidth = 601
    inherited ButtonOK: TcxButton
      Left = 431
      ExplicitLeft = 431
    end
    inherited ButtonCancel: TcxButton
      Left = 513
      ExplicitLeft = 513
    end
  end
  inherited LayoutControlHeader: TdxLayoutControl
    Width = 601
    ExplicitWidth = 601
    inherited LayoutItemHeader: TdxLayoutLabeledItem
      CaptionOptions.Text = 'Project languages'
    end
  end
  inherited LayoutControl: TdxLayoutControl
    Width = 601
    Height = 271
    ExplicitWidth = 601
    ExplicitHeight = 271
    object ComboBoxSourceLanguage: TcxExtLookupComboBox [0]
      Left = 104
      Top = 7
      RepositoryItem = DataModuleMain.EditRepositoryComboBoxItemLanguage
      Style.HotTrack = False
      TabOrder = 0
      Width = 199
    end
    object CheckBoxApplyFilter: TcxCheckBox [1]
      Left = 104
      Top = 188
      Action = ActionApplyFilter
      Style.HotTrack = False
      TabOrder = 1
      Transparent = True
    end
    object CheckBoxHideSupplemental: TcxCheckBox [2]
      Left = 104
      Top = 215
      Action = ActionHideSupplemental
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 2
      Transparent = True
    end
    inherited LayoutControlGroup_Root: TdxLayoutGroup
      AlignVert = avClient
      ItemIndex = 3
    end
    object LayoutItemSourceLanguage: TdxLayoutItem
      Parent = LayoutControlGroup_Root
      CaptionOptions.Text = '&Source language:'
      Control = ComboBoxSourceLanguage
      ControlOptions.AlignHorz = ahLeft
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 199
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutGroup1: TdxLayoutGroup
      Parent = LayoutControlGroup_Root
      AlignVert = avBottom
      CaptionOptions.Text = 'New Group'
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 5
    end
    object LayoutItemTargetLanguage: TdxLayoutItem
      Parent = LayoutControlGroup_Root
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.AlignVert = tavTop
      CaptionOptions.Text = '&Target languages:'
      ControlOptions.OriginalHeight = 249
      ControlOptions.OriginalWidth = 474
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem5: TdxLayoutItem
      Parent = LayoutControlGroup_Root
      CaptionOptions.ShowAccelChar = False
      CaptionOptions.Text = ' '
      Control = CheckBoxApplyFilter
      ControlOptions.OriginalHeight = 20
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutEmptySpaceItem1: TdxLayoutEmptySpaceItem
      Parent = LayoutControlGroup_Root
      SizeOptions.Height = 10
      SizeOptions.Width = 10
      CaptionOptions.Text = 'Empty Space Item'
      Index = 4
    end
    object dxLayoutItem1: TdxLayoutItem
      Parent = LayoutControlGroup_Root
      CaptionOptions.Text = ' '
      Control = CheckBoxHideSupplemental
      ControlOptions.OriginalHeight = 20
      ControlOptions.OriginalWidth = 269
      ControlOptions.ShowBorder = False
      Index = 3
    end
  end
  object ActionList1: TActionList
    Left = 296
    Top = 192
    object ActionApplyFilter: TAction
      AutoCheck = True
      Caption = '&Filter target language lists with above selection'
      OnExecute = ActionApplyFilterExecute
      OnUpdate = ActionApplyFilterUpdate
    end
    object ActionHideSupplemental: TAction
      AutoCheck = True
      Caption = '&Hide supplemental locales'
      Checked = True
      OnExecute = ActionHideSupplementalExecute
    end
  end
end
