-- The Village of Vampire by YT, このソースコードはNYSLです
procedure Vampire.R3.Log_Index_Page (
	Output : not null access Ada.Streams.Root_Stream_Type'Class;
	Form : in Forms.Root_Form_Type'Class;
	Template : in String;
	HTML_Directory : in String;
	Style_Sheet : in String;
	Background : in String;
	Summaries : in Tabula.Villages.Lists.Summary_Maps.Map);
