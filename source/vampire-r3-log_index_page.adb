-- The Village of Vampire by YT, このソースコードはNYSLです
procedure Vampire.R3.Log_Index_Page (
	Output : not null access Ada.Streams.Root_Stream_Type'Class;
	Form : in Forms.Root_Form_Type'Class;
	Template : in String;
	HTML_Directory : in String;
	Style_Sheet : in String;
	Background : in String;
	Summaries : in Tabula.Villages.Lists.Summary_Maps.Map)
is
	procedure Handle (
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		Tag : in String;
		Contents : in Web.Producers.Template) is
	begin
		if Tag = "background" then
			Forms.Write_Link (
				Output,
				Form,
				Name => "background",
				Current_Directory => HTML_Directory,
				Resource => Background);
		elsif Tag = "href_index" then
			Forms.Write_Link (
				Output,
				Form,
				Name => "href",
				Current_Directory => HTML_Directory,
				Resource => Forms.Self,
				Parameters =>
					Form.Parameters_To_Index_Page (
						User_Id => "",
						User_Password => ""));
		elsif Tag = "loglist" then
			Handle_Village_List (
				Output,
				Contents,
				Form,
				Current_Directory => HTML_Directory,
				HTML_Directory => HTML_Directory,
				Summaries => Summaries,
				Log => True,
				Limits => 9999,
				User_Id => "",
				User_Password => "");
		elsif Tag = "href_stylesheet" then
			Forms.Write_Link (
				Output,
				Form,
				Name => "href",
				Current_Directory => HTML_Directory,
				Resource => Style_Sheet);
		else
			Raise_Unknown_Tag (Tag);
		end if;
	end Handle;
begin
	Web.Producers.Produce (Output, Read (Template), Handler => Handle'Access);
end Vampire.R3.Log_Index_Page;
