-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Strings.Fixed;
with iconv.Streams;
package body Tabula.Renderers.Simple is
	use type Villages.Village_State;
	
	-- function "+" (S : Ada.Strings.Unbounded.Unbounded_String) return String renames Ada.Strings.Unbounded.To_String;
	-- function "+" (S : String) return Ada.Strings.Unbounded.Unbounded_String renames Ada.Strings.Unbounded.To_Unbounded_String;
	
	Encoding : aliased iconv.Encoding; -- may not be closed...
	Encoding_Ready : Boolean := False;

	function Ready_Encoding return not null access iconv.Encoding is
	begin
		if not Encoding_Ready then
			Encoding := iconv.Open(Encoded => "SJIS", Decoded => "UTF-8");
			Encoding_Ready := True;
		end if;
		return Encoding'Access;
	end Ready_Encoding;
	
	subtype Super is Renderers.Renderer;
	
	overriding function Get_Village_Id(
		Object : Renderer; 
		Query_Strings : Web.Query_Strings) return Villages.Village_Id
	is
		S : String renames Web.Element(Query_Strings, "v");
	begin
		if S'Length = Villages.Village_Id'Length then
			return S;
		else
			return Villages.Invalid_Village_Id;
		end if;
	end Get_Village_Id;

	overriding procedure Get_Day(
		Object : in Renderer; 
		Village : in Vampires.Villages.Village_Type; 
		Query_Strings : in Web.Query_Strings; 
		Day : out Natural)
	is
		S : String renames Web.Element(Query_Strings, "d");
	begin
		Day := Natural'Value(S);
	exception
		when Constraint_Error => 
			if Village.State /= Villages.Closed then
				Day := Village.Today;
			else
				Day := 0;
			end if;
	end Get_Day;
	
	overriding procedure Get_Range(
		Object : in Renderer; 
		Village : in Vampires.Villages.Village_Type; 
		Day : in Natural;
		Query_Strings : in Web.Query_Strings; 
		First, Last : out Integer)
	is
		Range_Arg : String renames Web.Element(Query_Strings, "r");
		P : constant Natural := Ada.Strings.Fixed.Index(Range_Arg, "-");
	begin
		if P < Range_Arg'First then
			Last := Vampires.Villages.Count_Speech(Village, Day);
			First := Last - (Natural'Value(Range_Arg));
		else
			First := Natural'Value(Range_Arg(Range_Arg'First .. P - 1));
			Last := Natural'Value(Range_Arg(P + 1 .. Range_Arg'Last));
		end if;
	exception
		when Constraint_Error => 
			if (Village.State /= Villages.Closed) and then (Day = Village.Today) then
				Last := Vampires.Villages.Count_Speech(Village, Day);
				First := Last - Speeches_By_Page;
			else
				First := 0;
				Last := Speeches_By_Page - 1;
			end if;
	end Get_Range;
	
	overriding function Get_User_Id(
		Object : Renderer; 
		Query_Strings : Web.Query_Strings;
		Cookie : Web.Cookie) return String is
	begin
		return Web.Element(Query_Strings, "i");
	end Get_User_Id;
	
	overriding function Get_User_Password(
		Object : Renderer; 
		Query_Strings : Web.Query_Strings;
		Cookie : Web.Cookie) return String is
	begin
		return Web.Element(Query_Strings, "p");
	end Get_User_Password;

	overriding procedure Set_User(
		Object : in Renderer; 
		Cookie : in out Web.Cookie;
		User_Id: in String;
		User_Password : in String) is
	begin
		null;
	end Set_User;
	
	overriding function Get_Text(
		Object : Renderer; 
		Inputs : Web.Query_Strings) return String is
	begin
		return Ada.Strings.Fixed.Trim(iconv.Decode(Ready_Encoding.all, Web.Element(Inputs, "text")), Ada.Strings.Both);
	end Get_Text;
	
	function Is_User_Page(
		Object : Renderer; 
		Query_Strings : Web.Query_Strings;
		Cookie : Web.Cookie) return Boolean 
	is
		User_Id : String renames Web.Element(Query_Strings, "i");
	begin
		if User_Id = "" then
			return False;
		else
			return Web.Element(Query_Strings, "u") = User_Id;
		end if;
	end Is_User_Page;
	
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
		Village_Id : Villages.Village_Id := Villages.Invalid_Village_Id;
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
		elsif Village_Id /= Villages.Invalid_Village_Id then
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

end Tabula.Renderers.Simple;
