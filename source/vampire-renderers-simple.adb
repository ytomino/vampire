-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Strings.Fixed;
with iconv.Streams;
package body Vampire.Renderers.Simple is
	use Tabula.Villages;
	use Villages;
	
	Encoding : access iconv.Encoding; -- may not be deallocation...

	function Ready_Encoding return not null access iconv.Encoding is
	begin
		if Encoding = null then
			Encoding := new iconv.Encoding'(
				iconv.Open (Encoded => "SJIS", Decoded => "UTF-8"));
		end if;
		return Encoding;
	end Ready_Encoding;
	
	subtype Super is Renderers.Renderer;
	
	overriding procedure Refresh_Page(
		Object : in Renderer; 
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		URI : in String) 
	is
		Encoder_Stream : aliased iconv.Streams.Stream := iconv.Streams.Create (Output, Ready_Encoding);
	begin
		Write(Encoder_Stream'Access, 
			"<html>" & 
			"<head>" &
			"<meta http-equiv=""CONTENT-TYPE"" content=""text/html; charset=SJIS"">" &
			"<title>" & "The Village of Vampire" & "</title>" &
			"</head>" &
			"<body>" &
			"<h1>" & "The Village of Vampire" & "</h1>" &
			"<hr>" &
			"<div>受理しました。<div>" &
			"<hr>" &
			"<div><a href=""");
		Write(Encoder_Stream'Access, URI);
		Write(Encoder_Stream'Access, 
			""">戻る</a><div>" &
			"</body>" &
			"</html>");
	end Refresh_Page;
	
	overriding procedure Produce(
		Object : in Renderer;
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		File_Name : in String;
		Handler : not null access procedure(Output : not null access Ada.Streams.Root_Stream_Type'Class; Tag : in String; Contents : Web.Producers.Template)) 
	is
		Encoder_Stream : aliased iconv.Streams.Stream := iconv.Streams.Create (Output, Ready_Encoding);
	begin
		Produce(Super(Object), Encoder_Stream'Access, File_Name, Handler);
	end Produce;
	
	overriding procedure Link(
		Object : in Renderer;
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		Village_Id : Tabula.Villages.Village_Id := Tabula.Villages.Invalid_Village_Id;
		Day : Integer := -1;
		First : Integer := -1;
		Last : Integer := -1;
		Latest : Integer := -1;
		Log : Boolean := False;
		User_Id : in String;
		User_Password : in String;
		User_Page : Boolean := False)
	is
		function Self_URI return String is
			URI : String renames Web.Request_URI;
			First, Last : Natural;
		begin
		 	Last := Ada.Strings.Fixed.Index(URI, "?") - 1;
			if Last < URI'First then
				Last := URI'Last;
			end if;
			First := 0;
			for I in reverse URI'First .. Last loop
				if URI(I) = '/' then
					First := I;
					exit;
				end if;
			end loop;
			if First < URI'First then
				First := URI'First;
			end if;
			return "." & URI(First .. Last);
		end Self_URI;
	begin
		Write(Output, '"');
		Write(Output, Self_URI);
		Write(Output, "?b=k");
		if User_Id /= "" then
			Write(Output, "&i=");
			Write(Output, User_Id);
			Write(Output, "&p=");
			Write(Output, Web.Encode_URI(User_Password));
		end if;
		if User_Page then
			Write(Output, "&u=");
			Write(Output, User_Id);
		elsif Village_Id /= Invalid_Village_Id then
			Write(Output, "&v=");
			Write(Output, Village_Id);
			if Day >= 0 then
				Write(Output, "&d=");
				Write(Output, To_String(Day));
			end if;
			if First >= 0 and then Last >= 0 then
				Write(Output, "&r=");
				Write(Output, To_String(First));
				Write(Output, '-');
				Write(Output, To_String(Last));
			elsif Latest >= 0 then
				Write(Output, "&r=");
				Write(Output, To_String(Latest));
			end if;
		end if;
		Write(Output, '"');
	end Link;
	
	overriding function HTML_Version(Object : in Renderer) return Web.HTML_Version is
	begin
		return Web.HTML;
	end HTML_Version;

end Vampire.Renderers.Simple;
