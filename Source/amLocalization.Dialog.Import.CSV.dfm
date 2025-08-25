object FormCSVImport: TFormCSVImport
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Delimited Text Import'
  ClientHeight = 462
  ClientWidth = 597
  Color = clBtnFace
  Constraints.MinHeight = 400
  Constraints.MinWidth = 500
  ParentFont = True
  Position = poMainFormCenter
  ShowHint = True
  TextHeight = 15
  object WizardControl: TdxWizardControl
    Left = 0
    Top = 0
    Width = 597
    Height = 462
    Buttons.CustomButtons.Buttons = <>
    Buttons.Help.Visible = False
    InfoPanel.Caption = 'More options...'
    InfoPanel.Hyperlink = 'lorem ipsum'
    OptionsViewStyleAero.Title.Text = 'Import translations from delimited text file'
    ViewStyle = wcvsAero
    OnButtonClick = WizardControlButtonClick
    OnInfoPanelClick = WizardControlInfoPanelClick
    OnPageChanging = WizardControlPageChanging
    object WizardControlPageFile: TdxWizardControlPage
      Header.Title = 'Select source file'
      object LayoutControlFile: TdxLayoutControl
        Left = 0
        Top = 0
        Width = 550
        Height = 293
        Align = alClient
        ParentBackground = True
        TabOrder = 0
        Transparent = True
        LayoutLookAndFeel = DataModuleMain.LayoutSkinLookAndFeel
        DesignSize = (
          550
          293)
        object EditFilename: TcxButtonEdit
          Left = 66
          Top = 7
          Anchors = [akLeft, akTop, akRight]
          Properties.Buttons = <
            item
              Action = ActionFileBrowse
              Default = True
              Kind = bkGlyph
            end>
          Properties.Images = DataModuleMain.ImageListSmall
          Properties.OnEditValueChanged = EditFilenamePropertiesEditValueChanged
          TabOrder = 0
          TextHint = 'Enter the name of the file to import from'
          Width = 477
        end
        object ComboBoxEncoding: TcxLookupComboBox
          Left = 66
          Top = 37
          Properties.DropDownAutoSize = True
          Properties.DropDownListStyle = lsFixedList
          Properties.DropDownSizeable = True
          Properties.ImmediatePost = True
          Properties.KeyFieldNames = 'Codepage'
          Properties.ListColumns = <
            item
              Caption = 'Name'
              MinWidth = 50
              Width = 100
              FieldName = 'Name'
            end
            item
              Caption = 'Codepage'
              HeaderAlignment = taRightJustify
              MinWidth = 30
              Width = 60
              FieldName = 'Codepage'
            end>
          Properties.ListOptions.CaseInsensitive = True
          Properties.ListSource = DataSourceCodePages
          Properties.OnEditValueChanged = ComboBoxEncodingPropertiesEditValueChanged
          Style.HotTrack = False
          Style.TransparentBorder = False
          TabOrder = 1
          Width = 145
        end
        object ButtonEncoding: TcxButton
          Left = 218
          Top = 37
          Width = 24
          Height = 21
          Action = ActionFileEncodingWarning
          OptionsImage.Spacing = 0
          PaintStyle = bpsGlyph
          SpeedButtonOptions.CanBeFocused = False
          SpeedButtonOptions.AllowAllUp = True
          SpeedButtonOptions.Flat = True
          SpeedButtonOptions.Transparent = True
          TabOrder = 2
        end
        object MemoFilePreview: TcxMemo
          Left = 7
          Top = 99
          TabStop = False
          ParentFont = False
          Properties.ReadOnly = True
          Properties.ScrollBars = ssBoth
          Properties.WantReturns = False
          Properties.WordWrap = False
          Style.Font.Charset = DEFAULT_CHARSET
          Style.Font.Color = clWindowText
          Style.Font.Height = -11
          Style.Font.Name = 'Courier New'
          Style.Font.Style = []
          Style.HotTrack = False
          Style.TransparentBorder = False
          Style.IsFontAssigned = True
          TabOrder = 3
          Height = 187
          Width = 536
        end
        object LayoutControlFileGroup_Root: TdxLayoutGroup
          AlignHorz = ahClient
          AlignVert = avClient
          Hidden = True
          ItemIndex = 1
          ShowBorder = False
          Index = -1
        end
        object LayoutItemFileName: TdxLayoutItem
          Parent = LayoutControlFileGroup_Root
          CaptionOptions.Text = '&Filename:'
          Control = EditFilename
          ControlOptions.OriginalHeight = 23
          ControlOptions.OriginalWidth = 587
          ControlOptions.ShowBorder = False
          Index = 0
        end
        object LayoutItemFileEncoding: TdxLayoutItem
          Parent = dxLayoutGroup1
          AlignHorz = ahLeft
          CaptionOptions.Text = 'Encoding:'
          Control = ComboBoxEncoding
          ControlOptions.OriginalHeight = 21
          ControlOptions.OriginalWidth = 145
          ControlOptions.ShowBorder = False
          Index = 0
        end
        object LayoutItemFileEncodingWarning: TdxLayoutItem
          Parent = dxLayoutGroup1
          AlignHorz = ahLeft
          Visible = False
          CaptionOptions.Visible = False
          Control = ButtonEncoding
          ControlOptions.OriginalHeight = 21
          ControlOptions.OriginalWidth = 24
          ControlOptions.ShowBorder = False
          Index = 1
        end
        object dxLayoutGroup1: TdxLayoutGroup
          Parent = LayoutControlFileGroup_Root
          CaptionOptions.Text = 'New Group'
          ItemIndex = 1
          LayoutDirection = ldHorizontal
          ShowBorder = False
          Index = 1
        end
        object LayoutItemFilePreview: TdxLayoutItem
          Parent = LayoutControlFileGroup_Root
          AlignVert = avClient
          CaptionOptions.Text = 'Preview:'
          CaptionOptions.Layout = clTop
          Control = MemoFilePreview
          ControlOptions.OriginalHeight = 89
          ControlOptions.OriginalWidth = 185
          ControlOptions.ShowBorder = False
          Index = 3
        end
        object dxLayoutSeparatorItem1: TdxLayoutSeparatorItem
          Parent = LayoutControlFileGroup_Root
          CaptionOptions.Text = 'Separator'
          Index = 2
        end
      end
    end
    object WizardControlPageLayout: TdxWizardControlPage
      Header.Title = 'File layout'
      object LayoutControlLayout: TdxLayoutControl
        Left = 0
        Top = 0
        Width = 550
        Height = 293
        Align = alClient
        ParentBackground = True
        TabOrder = 0
        Transparent = True
        LayoutLookAndFeel = DataModuleMain.LayoutSkinLookAndFeel
        object ComboBoxDelimiter: TcxComboBox
          Left = 111
          Top = 7
          Properties.ImmediatePost = True
          Properties.Items.Strings = (
            'Semicolon'
            'Tab'
            'Comma'
            'Space')
          Properties.OnEditValueChanged = ComboBoxDelimiterPropertiesEditValueChanged
          Style.HotTrack = False
          Style.TransparentBorder = False
          TabOrder = 0
          Text = 'Semicolon'
          Width = 76
        end
        object ComboBoxDecimal: TcxComboBox
          Left = 111
          Top = 35
          Properties.DropDownListStyle = lsFixedList
          Properties.Items.Strings = (
            'Comma'
            'Period')
          Style.HotTrack = False
          Style.TransparentBorder = False
          TabOrder = 1
          Text = 'Comma'
          Width = 76
        end
        object ImageDecimalWarning: TImage
          Left = 194
          Top = 35
          Width = 20
          Height = 16
          Hint = 'The field delimiter is the same as the decimal separator'
          Picture.Data = {
            0B546478504E47496D61676589504E470D0A1A0A0000000D4948445200000010
            0000001008060000001FF3FF610000000467414D410000AFC837058AE9000000
            1974455874536F6674776172650041646F626520496D616765526561647971C9
            653C0000024949444154384FA5925D4853611CC6070321B02083BA88206A1521
            A292998DF42AE842880A6B63AD2EC20B83BAF02ABC28221083A42E5C52548233
            0AAD286BF661D18C4169DFE93ECE3E74D671DF5B6E67E7AC6DE7EC3CBDE7244B
            DDB9AA8B1FBCEFC3F33C7FDE0F1580FF425194885B1BD5B1577BECD151AD2DFC
            7CB75AC923A1284A90F0D594BD13C96F17101AD97552C923A1289270456454CB
            14F229E4E71D083EDE99987BB8A342C95B2248445E682D0C658228A421E64348
            4E5D017DBFB657C95B2290704DF8594301851C721113F2B13E5212C7F7C16A61
            F64E55CD72FF928D0409DB59FF20C927E01C689211383B522E3366062A6DCBFD
            4B36E1A70D86C4783B20E6C0FA8EC3616E9261DD46887C0281976DF0DEDC6A58
            9C591C2E0F5AEAE3BFA2E328E483603DC7E0E86F94615C3A08E97760E931B8AF
            6BE6A8DE4DE52505C127F543C9C96E32298A6CD0440A8C981EDEB75070049CD7
            88427606A1B1B3B05FDED0B5A480843581E13A5EE4D3E0191B323EA35C90F1B7
            22F64107C679189CBB05D990093C4BE36BD7BADCA7F36B34C582C0A3BAF729D7
            2D72712164039790F11A48C1D1BF4770B680A30E81751E8090A110B05EC444C7
            2A8B5C40C2CD61EB09F2E62CF93423E03C7A99344526BBF424AC2797A8230507
            C1BAF623E3215E72CCC99EBDB09D2E6B56D10F6A698E7E2D1788FC3CE1E71F04
            B296286A8905E2446710FB7C0F6FDAD494EAC750B579F66E15FCB72B31DDBF1D
            BEBE6DF0DCD802F7B5CD70F66CC454F77A7CE95C8B8FE75663E2CC4ABC6D5F01
            DBA932290C6BABCA5C7C857F03AADF463000A5024714180000000049454E44AE
            426082}
        end
        object SpinEditFirstRow: TcxSpinEdit
          Left = 111
          Top = 63
          Properties.Alignment.Horz = taRightJustify
          Properties.ImmediatePost = True
          Properties.MaxValue = 100.000000000000000000
          Properties.MinValue = 1.000000000000000000
          Properties.UseLeftAlignmentOnEditing = False
          Properties.OnEditValueChanged = SpinEditFirstRowPropertiesEditValueChanged
          Style.HotTrack = False
          Style.TransparentBorder = False
          TabOrder = 2
          Value = 1
          Width = 76
        end
        object GridLayout: TcxGrid
          Left = 7
          Top = 161
          Width = 536
          Height = 125
          TabOrder = 4
          object GridLayoutTableView: TcxGridTableView
            Navigator.Buttons.CustomButtons = <>
            ScrollbarAnnotations.CustomAnnotations = <>
            DataController.Summary.DefaultGroupSummaryItems = <>
            DataController.Summary.FooterSummaryItems = <>
            DataController.Summary.SummaryGroups = <>
            OptionsBehavior.AlwaysShowEditor = True
            OptionsBehavior.CellHints = True
            OptionsCustomize.ColumnFiltering = False
            OptionsCustomize.ColumnGrouping = False
            OptionsCustomize.ColumnMoving = False
            OptionsCustomize.ColumnSorting = False
            OptionsCustomize.ColumnsQuickCustomizationShowCommands = False
            OptionsData.Deleting = False
            OptionsData.Inserting = False
            OptionsView.CellEndEllipsis = True
            OptionsView.GroupByBox = False
            OptionsView.HeaderEndEllipsis = True
            Styles.OnGetContentStyle = GridLayoutTableViewStylesGetContentStyle
            OnColumnHeaderClick = GridLayoutTableViewColumnHeaderClick
          end
          object GridLayoutLevel: TcxGridLevel
            GridView = GridLayoutTableView
          end
        end
        object ComboBoxEscapeChar: TcxComboBox
          Left = 111
          Top = 91
          Properties.ImmediatePost = True
          Properties.Items.Strings = (
            '(none)'
            'Backslash')
          Properties.OnEditValueChanged = ComboBoxEscapeCharPropertiesEditValueChanged
          Style.HotTrack = False
          Style.TransparentBorder = False
          TabOrder = 3
          Text = '(none)'
          Width = 76
        end
        object LayoutControlLayoutGroup_Root: TdxLayoutGroup
          AlignHorz = ahClient
          AlignVert = avClient
          Hidden = True
          ShowBorder = False
          Index = -1
        end
        object LayoutItemLayoutDelimiter: TdxLayoutItem
          Parent = dxLayoutGroup3
          AlignHorz = ahLeft
          CaptionOptions.Text = '&Field delimiter:'
          Control = ComboBoxDelimiter
          ControlOptions.OriginalHeight = 21
          ControlOptions.OriginalWidth = 76
          ControlOptions.ShowBorder = False
          Index = 0
        end
        object LayoutItemLayoutDecimal: TdxLayoutItem
          Parent = LayoutGroupLayoutDecimal
          AlignHorz = ahLeft
          CaptionOptions.Text = '&Decimal separator:'
          Control = ComboBoxDecimal
          ControlOptions.OriginalHeight = 21
          ControlOptions.OriginalWidth = 76
          ControlOptions.ShowBorder = False
          Index = 0
        end
        object LayoutItemLayoutDecimalWarning: TdxLayoutItem
          Parent = LayoutGroupLayoutDecimal
          Control = ImageDecimalWarning
          ControlOptions.OriginalHeight = 16
          ControlOptions.OriginalWidth = 20
          ControlOptions.ShowBorder = False
          Index = 1
        end
        object LayoutGroupLayoutDecimal: TdxLayoutGroup
          Parent = dxLayoutGroup3
          CaptionOptions.Text = 'New Group'
          Visible = False
          LayoutDirection = ldHorizontal
          ShowBorder = False
          Index = 1
        end
        object LayoutItemLayoutFirstRow: TdxLayoutItem
          Parent = dxLayoutGroup3
          AlignHorz = ahLeft
          CaptionOptions.Text = 'F&irst row:'
          Control = SpinEditFirstRow
          ControlOptions.OriginalHeight = 21
          ControlOptions.OriginalWidth = 76
          ControlOptions.ShowBorder = False
          Index = 2
        end
        object dxLayoutSeparatorItem2: TdxLayoutSeparatorItem
          Parent = LayoutControlLayoutGroup_Root
          CaptionOptions.Text = 'Separator'
          Index = 1
        end
        object LayoutItemLayoutFields: TdxLayoutItem
          Parent = LayoutControlLayoutGroup_Root
          AlignVert = avClient
          CaptionOptions.Visible = False
          Control = GridLayout
          ControlOptions.OriginalHeight = 200
          ControlOptions.OriginalWidth = 250
          ControlOptions.ShowBorder = False
          Index = 2
        end
        object LayoutLabeledItemLayoutMap: TdxLayoutLabeledItem
          Parent = dxLayoutGroup4
          CaptionOptions.Text = '&Mapping:'
          Index = 0
        end
        object dxLayoutSeparatorItem4: TdxLayoutSeparatorItem
          Parent = dxLayoutGroup5
          CaptionOptions.Text = 'Separator'
          Index = 1
        end
        object LayoutCheckBoxItemMatchFuzzy: TdxLayoutCheckBoxItem
          Parent = dxLayoutGroup7
          CaptionOptions.Hint = 'Match on normalized values or only exact values'
          CaptionOptions.Text = 'Fu&zzy matching'
          State = cbsChecked
          Index = 1
        end
        object LayoutCheckBoxItemMatchAll: TdxLayoutCheckBoxItem
          Parent = dxLayoutGroup7
          CaptionOptions.Hint = 
            'Update all items with matching source value or only the best mat' +
            'ching one'
          CaptionOptions.Text = '&Update all matching'
          State = cbsChecked
          Index = 0
        end
        object dxLayoutSeparatorItem5: TdxLayoutSeparatorItem
          Parent = dxLayoutGroup5
          CaptionOptions.Text = 'Separator'
          Index = 3
        end
        object LayoutRadioButtonItemMapID: TdxLayoutRadioButtonItem
          Parent = LayoutGroupLayoutMap
          CaptionOptions.Hint = 'Text file contains source ID and target language values'
          CaptionOptions.Text = 'Map by ID'
          Checked = True
          TabStop = True
          OnClick = LayoutRadioButtonItemMapIDClick
          Index = 0
        end
        object LayoutRadioButtonItemMapBoth: TdxLayoutRadioButtonItem
          Parent = LayoutGroupLayoutMap
          CaptionOptions.Hint = 
            'Text file contains source ID and target language values. Use sou' +
            'rce value if ID isn'#39't found'
          CaptionOptions.Text = 'Map by ID, then value'
          OnClick = LayoutRadioButtonItemMapBothClick
          Index = 2
        end
        object LayoutRadioButtonItemMapValue: TdxLayoutRadioButtonItem
          Parent = LayoutGroupLayoutMap
          CaptionOptions.Hint = 'Text file contains source and target language values'
          CaptionOptions.Text = 'Map by value'
          OnClick = LayoutRadioButtonItemMapValueClick
          Index = 1
        end
        object dxLayoutGroup3: TdxLayoutGroup
          Parent = dxLayoutGroup5
          CaptionOptions.Text = 'New Group'
          ShowBorder = False
          Index = 0
        end
        object LayoutGroupLayoutMap: TdxLayoutGroup
          Parent = dxLayoutGroup4
          CaptionOptions.Text = 'Mapping'
          ShowBorder = False
          Index = 1
        end
        object dxLayoutGroup5: TdxLayoutGroup
          Parent = LayoutControlLayoutGroup_Root
          CaptionOptions.Text = 'New Group'
          LayoutDirection = ldHorizontal
          ScrollOptions.Horizontal = smIndependent
          ShowBorder = False
          Index = 0
        end
        object dxLayoutGroup4: TdxLayoutGroup
          Parent = dxLayoutGroup6
          CaptionOptions.Text = 'New Group'
          LayoutDirection = ldHorizontal
          ShowBorder = False
          Index = 0
        end
        object dxLayoutGroup6: TdxLayoutGroup
          Parent = dxLayoutGroup5
          CaptionOptions.Text = 'New Group'
          ShowBorder = False
          Index = 2
        end
        object dxLayoutGroup7: TdxLayoutGroup
          Parent = dxLayoutGroup5
          CaptionOptions.Text = 'New Group'
          ShowBorder = False
          Index = 4
        end
        object LayoutItemLayoutEscapeChar: TdxLayoutItem
          Parent = dxLayoutGroup3
          AlignHorz = ahLeft
          Visible = False
          CaptionOptions.Text = 'Escape char:'
          Control = ComboBoxEscapeChar
          ControlOptions.OriginalHeight = 21
          ControlOptions.OriginalWidth = 76
          ControlOptions.ShowBorder = False
          Index = 3
        end
      end
    end
    object WizardControlPageImport: TdxWizardControlPage
      Header.Title = 'Import translations'
      object ButtonImport: TcxButton
        AlignWithMargins = True
        Left = 32
        Top = 32
        Width = 518
        Height = 72
        Margins.Left = 32
        Margins.Top = 32
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alTop
        Caption = 'Start import'
        Constraints.MaxWidth = 518
        Description = 'Update translations with values from the import file'
        Kind = cxbkCommandLink
        TabOrder = 0
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
        OnClick = ButtonImportClick
      end
    end
    object WizardControlPageProgress: TdxWizardControlPage
      Header.Title = 'Importing...'
      object ListViewProgress: TcxListView
        Left = 0
        Top = 0
        Width = 550
        Height = 293
        Align = alClient
        ColumnClick = False
        Columns = <
          item
            AutoSize = True
          end>
        ReadOnly = True
        ShowColumnHeaders = False
        SmallImages = DataModuleMain.ImageListSmall
        Style.BorderStyle = cbsNone
        Style.TransparentBorder = False
        StyleFocused.BorderStyle = cbsNone
        StyleHot.BorderStyle = cbsNone
        TabOrder = 0
        ViewStyle = vsReport
      end
    end
    object WizardControlPageDone: TdxWizardControlPage
      Header.AssignedValues = [wchvDescriptionVisibility, wchvDescriptionOffset]
      Header.DescriptionOffset = 0
      Header.DescriptionVisibility = wcevDefault
      Header.Title = 'Import completed'
      Header.Description = ''
      object LayoutControlDone: TdxLayoutControl
        Left = 0
        Top = 0
        Width = 550
        Height = 293
        Align = alClient
        ParentBackground = True
        TabOrder = 0
        Transparent = True
        LayoutLookAndFeel = DataModuleMain.LayoutSkinLookAndFeel
        object LabelCountAdded: TcxLabel
          Left = 73
          Top = 30
          AutoSize = False
          Caption = '1000'
          Style.HotTrack = False
          Style.TransparentBorder = False
          Properties.Alignment.Horz = taRightJustify
          Properties.ShowAccelChar = False
          TabOrder = 0
          Transparent = True
          Height = 16
          Width = 50
          AnchorX = 123
        end
        object LabelCountUpdated: TcxLabel
          Left = 73
          Top = 53
          AutoSize = False
          Caption = '0'
          Style.HotTrack = False
          Style.TransparentBorder = False
          Properties.Alignment.Horz = taRightJustify
          Properties.ShowAccelChar = False
          TabOrder = 1
          Transparent = True
          Height = 16
          Width = 50
          AnchorX = 123
        end
        object LabelCountSkipped: TcxLabel
          Left = 73
          Top = 76
          AutoSize = False
          Caption = '0'
          Style.HotTrack = False
          Style.TransparentBorder = False
          Properties.Alignment.Horz = taRightJustify
          Properties.ShowAccelChar = False
          TabOrder = 2
          Transparent = True
          Height = 16
          Width = 50
          AnchorX = 123
        end
        object LayoutControlDoneGroup_Root: TdxLayoutGroup
          AlignHorz = ahClient
          AlignVert = avClient
          Hidden = True
          ItemIndex = 3
          ShowBorder = False
          Index = -1
        end
        object LayoutLabeledItemCount: TdxLayoutLabeledItem
          Parent = LayoutControlDoneGroup_Root
          CaptionOptions.ShowAccelChar = False
          CaptionOptions.Text = 'Translations have been updated with the following changes:'
          Index = 0
        end
        object LayoutItemCountAdded: TdxLayoutItem
          Parent = dxLayoutGroup2
          AlignHorz = ahLeft
          CaptionOptions.Text = 'Added:'
          Control = LabelCountAdded
          ControlOptions.OriginalHeight = 16
          ControlOptions.OriginalWidth = 50
          ControlOptions.ShowBorder = False
          Index = 0
        end
        object LayoutItemCountUpdated: TdxLayoutItem
          Parent = dxLayoutGroup2
          AlignHorz = ahLeft
          CaptionOptions.ShowAccelChar = False
          CaptionOptions.Text = 'Updated:'
          Control = LabelCountUpdated
          ControlOptions.OriginalHeight = 16
          ControlOptions.OriginalWidth = 50
          ControlOptions.ShowBorder = False
          Index = 1
        end
        object LayoutItemCountSkipped: TdxLayoutItem
          Parent = dxLayoutGroup2
          AlignHorz = ahLeft
          CaptionOptions.ShowAccelChar = False
          CaptionOptions.Text = 'Skipped:'
          Control = LabelCountSkipped
          ControlOptions.OriginalHeight = 16
          ControlOptions.OriginalWidth = 50
          ControlOptions.ShowBorder = False
          Index = 2
        end
        object dxLayoutGroup2: TdxLayoutGroup
          Parent = LayoutControlDoneGroup_Root
          CaptionOptions.Text = 'New Group'
          Offsets.Left = 12
          ItemIndex = 2
          ShowBorder = False
          Index = 1
        end
        object dxLayoutSeparatorItem3: TdxLayoutSeparatorItem
          Parent = LayoutControlDoneGroup_Root
          CaptionOptions.Text = 'Separator'
          Index = 2
        end
        object LayoutItemResultWarnings: TdxLayoutItem
          Parent = LayoutControlDoneGroup_Root
          AlignHorz = ahClient
          AlignVert = avClient
          Visible = False
          CaptionOptions.ShowAccelChar = False
          CaptionOptions.Text = 'Errors and warnings:'
          CaptionOptions.Layout = clTop
          Index = 3
        end
      end
    end
  end
  object ActionList: TActionList
    Images = DataModuleMain.ImageListSmall
    Left = 308
    Top = 236
    object ActionFileBrowse: TAction
      Hint = 'Browse for file'
      ImageIndex = 0
      OnExecute = ActionFileBrowseExecute
    end
    object ActionFileEncodingWarning: TAction
      ImageIndex = 84
      OnExecute = ActionFileEncodingWarningExecute
    end
  end
  object FDMemTableCodePages: TFDMemTable
    Active = True
    FieldDefs = <
      item
        Name = 'CodePage'
        DataType = ftInteger
      end
      item
        Name = 'Name'
        DataType = ftString
        Size = 100
      end>
    IndexDefs = <>
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired, uvAutoCommitUpdates]
    UpdateOptions.CheckRequired = False
    UpdateOptions.AutoCommitUpdates = True
    StoreDefs = True
    Left = 72
    Top = 415
    object FDMemTableCodePagesFDMemTableCodePagesFieldCodePage: TIntegerField
      FieldName = 'CodePage'
    end
    object FDMemTableCodePagesFDMemTableCodePagesFieldName: TStringField
      FieldName = 'Name'
      Size = 100
    end
  end
  object DataSourceCodePages: TDataSource
    DataSet = FDMemTableCodePages
    Left = 196
    Top = 416
  end
  object EditRepository: TcxEditRepository
    Left = 356
    Top = 415
    PixelsPerInch = 96
    object EditRepositoryDataItem: TcxEditRepositoryLabel
      Properties.ShowAccelChar = False
      Properties.ShowEndEllipsis = True
    end
  end
  object PopupMenuColumn: TPopupMenu
    AutoHotkeys = maManual
    AutoPopup = False
    Left = 276
    Top = 416
  end
end
