-- The Village of Vampire by YT, このソースコードはNYSLです
with Tabula.Users;
procedure Vampire.R3.User_Page (
	Output : not null access Ada.Streams.Root_Stream_Type'Class;
	Form : in Forms.Root_Form_Type'Class;
	Template : in String;
	Summaries : in Tabula.Villages.Lists.Summary_Maps.Map;
	User_Id : in String;
	User_Password : in String;
	User_Info : in Users.User_Info);
