-- The Village of Vampire by YT, このソースコードはNYSLです
with Tabula.Users.Lists;
with Tabula.Villages.Lists;
with Vampire.Forms;
procedure Vampire.R3.User_List_Page (
	Output : not null access Ada.Streams.Root_Stream_Type'Class;
	Form : in Forms.Root_Form_Type'Class;
	Template : in String;
	HTML_Directory : in String;
	Summaries : in Tabula.Villages.Lists.Summary_Maps.Map;
	User_List : in Users.Lists.User_Info_Maps.Map;
	User_Id : in String;
	User_Password : in String);
