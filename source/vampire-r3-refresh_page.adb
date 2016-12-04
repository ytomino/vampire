-- The Village of Vampire by YT, このソースコードはNYSLです
procedure Vampire.R3.Refresh_Page (
	Output : not null access Ada.Streams.Root_Stream_Type'Class;
	Form : in Forms.Root_Form_Type'Class;
	URI : in String) is
begin
	case Form.Template_Set is
		when Forms.For_Full =>
			String'Write (
				Output,
				"<meta http-equiv=""REFRESH"" content=""0;URL=");
			Forms.Write_In_Attribute (Output, Form, URI);
			String'Write (
				Output,
				""" />" &
				"<style>body{background-color:black;}</style>");
		when Forms.For_Mobile =>
			String'Write (
				Output,
				"<html>" & 
				"<head>" &
				"<meta http-equiv=""CONTENT-TYPE"" content=""text/html; charset=SJIS"">" &
				"<title>" & "The Village of Vampire" & "</title>" &
				"</head>" &
				"<body>" &
				"<h1>" & "The Village of Vampire" & "</h1>" &
				"<hr>" &
				"<div>");
			Forms.Write_In_HTML (Output, Form, "受理しました。");
			String'Write (
				Output,
				"</div>" &
				"<hr>" &
				"<div><a href=""");
			Forms.Write_In_Attribute (Output, Form, URI);
			String'Write (
				Output,
				""">");
			Forms.Write_In_HTML (Output, Form, "戻る");
			String'Write (
				Output,
				"</a></div>" &
				"</body>" &
				"</html>");
	end case;
end Vampire.R3.Refresh_Page;
