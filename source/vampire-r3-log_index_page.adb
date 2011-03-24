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
		Template : in Web.Producers.Template) is
	begin
		if Tag = "background" then
			String'Write (Output, "background=");
			Forms.Write_Link (
				Output,
				Form,
				Current_Directory => HTML_Directory,
				Resource => Background);
		elsif Tag = "href_index" then
			String'Write (Output, "href=");
			Forms.Write_Link (
				Output,
				Form,
				Current_Directory => HTML_Directory,
				Resource => Forms.Self,
				Parameters => Form.Parameters_To_Index_Page (
					User_Id => "",
					User_Password => ""));
		elsif Tag = "log_list" then
			Handle_Village_List (
				Output,
				Template,
				Form,
				Current_Directory => HTML_Directory,
				HTML_Directory => HTML_Directory,
				Summaries => Summaries,
				Log => True,
				Limits => 9999,
				User_Id => "",
				User_Password => "");
		elsif Tag = "href_stylesheet" then
			String'Write (Output, "href=");
			Forms.Write_Link (
				Output,
				Form,
				Current_Directory => HTML_Directory,
				Resource => Style_Sheet);
		else
			raise Program_Error with "Invalid template """ & Tag & """";
		end if;
	end Handle;
begin
	Produce (Output, Template, Handler => Handle'Access);
end Vampire.R3.Log_Index_Page;
