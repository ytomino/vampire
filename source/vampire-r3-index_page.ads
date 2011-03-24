-- The Village of Vampire by YT, このソースコードはNYSLです
procedure Vampire.R3.Index_Page (
	Output : not null access Ada.Streams.Root_Stream_Type'Class;
	Form : in Forms.Root_Form_Type'Class;
	Template : in String;
	HTML_Directory : in String;
	Summaries : in out Tabula.Villages.Lists.Summary_Maps.Map;
	Muramura : in Natural;
	User_Id: in String;
	User_Password : in String);
