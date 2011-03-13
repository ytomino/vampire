-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Calendar.Formatting;
with Ada.Streams.Stream_IO;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Tabula.Calendar;
with Tabula.Users;
package body Tabula.Renderers is
	use type Villages.Village_State;
	
	function "+" (S : Ada.Strings.Unbounded.Unbounded_String) return String renames Ada.Strings.Unbounded.To_String;
	
	function To_String(X : Integer) return String is
		Result : String := Integer'Image(X);
	begin
		if Result (Result'First) = ' ' then
			return Result (Result'First + 1 .. Result'Last);
		else
			return Result;
		end if;
	end To_String;
	
	function Name(Person : Vampires.Villages.Person_Type) return String is
	begin
		return (+Person.Work) & (+Person.Name);
	end Name;
	
	procedure Handle_List(
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		Tag : in String;
		Template : in Web.Producers.Template;
		Object : in Renderer;
		Summaries : in Villages.Lists.Summary_Maps.Map;
		Log_Limits : in Natural;
		User_Id, User_Password : in String)
	is
		Log : Boolean;
		procedure Handle_Villages(Output : not null access Ada.Streams.Root_Stream_Type'Class;
			Tag : in String; Template : in Web.Producers.Template)
		is
			I : Villages.Lists.Summary_Maps.Cursor;
		begin
			I := Summaries.First;
			if Log then
				declare
					C : Natural := Log_Limits;
					J : Villages.Lists.Summary_Maps.Cursor := Summaries.Last;
				begin
					while Villages.Lists.Summary_Maps.Has_Element (J) loop
						if Summaries.Constant_Reference (J).Element.State = Villages.Closed then
							C := C - 1;
							if C = 0 then
								I := J;
								exit;
							end if;
						end if;
						Villages.Lists.Summary_Maps.Previous (J);
					end loop;
				end;
			end if;
			while Villages.Lists.Summary_Maps.Has_Element (I) loop
				declare
					pragma Warnings (Off);
					Ref : constant Villages.Lists.Summary_Maps.Constant_Reference_Type :=
						Summaries.Constant_Reference (I);
					pragma Warnings (On);
					procedure Handle_Village(Output : not null access Ada.Streams.Root_Stream_Type'Class;
						Tag : in String; Template : in Web.Producers.Template) is
					begin
						if Tag = "id" then
							Write (Output, Ref.Key.all);
						elsif Tag = "name" then
							Write (Output, "<a href=");
							Link (Renderer'Class (Object), Output, Ref.Key.all, Log => Log,
								User_Id => User_Id, User_Password => User_Password);
							Write(Output, ">");
							if Ref.Element.Day_Duration < 24 * 60 * 60.0 then
								Write(Output, "短期 ");
							end if;
							Web.Write_In_HTML (
								Output,
								Renderer'Class(Object).HTML_Version,
								Ref.Element.Name.Constant_Reference.Element.all);
							Write(Output, "</a>");
						elsif Tag = "people" then
							Write(Output, To_String(Natural(Ref.Element.People.Length)) & "人");
						elsif Tag = "day" then
							Day_Name(Renderer'Class(Object), Output, Ref.Element.Today, Ref.Element.Today, Ref.Element.State);
						else
							raise Program_Error with "Invalid template """ & Tag & """";
						end if;
					end Handle_Village;
				begin
					if (Ref.Element.State = Villages.Closed) = Log then
						Web.Producers.Produce(Output, Template, Handler => Handle_Village'Access);
					end if;
				end;
				Villages.Lists.Summary_Maps.Next (I);
			end loop;
		end Handle_Villages;
	begin
		if Tag = "villages" then
			declare
				I : Villages.Lists.Summary_Maps.Cursor;
			begin
				I := Summaries.First;
				while Villages.Lists.Summary_Maps.Has_Element (I) loop
					declare
						pragma Warnings (Off);
						Ref : constant Villages.Lists.Summary_Maps.Constant_Reference_Type :=
							Summaries.Constant_Reference (I);
						pragma Warnings (On);
					begin
						if Ref.Element.State /= Villages.Closed then
							Log := False;
							Web.Producers.Produce(Output, Template, Handler => Handle_Villages'Access);
							exit;
						end if;
					end;
				end loop;
				Villages.Lists.Summary_Maps.Next (I);
			end;
		elsif Tag = "log" then
			declare
				I : Villages.Lists.Summary_Maps.Cursor;
			begin
				I := Summaries.First;
				while Villages.Lists.Summary_Maps.Has_Element (I) loop
					declare
						pragma Warnings (Off);
						Ref : constant Villages.Lists.Summary_Maps.Constant_Reference_Type :=
							Summaries.Constant_Reference (I);
						pragma Warnings (On);
					begin
						if Ref.Element.State = Villages.Closed then
							Log := True;
							Web.Producers.Produce(Output, Template, Handler => Handle_Villages'Access);
							exit;
						end if;
					end;
				end loop;
				Villages.Lists.Summary_Maps.Next (I);
			end;
		elsif Tag = "stylesheet" then
			Write(Output, "<link rel=""stylesheet"" type=""text/css"" href=");
			Link_Style_Sheet(Renderer'Class(Object), Output);
			Write(Output, "/>");
		elsif Tag = "background" then
			Link_Image(Renderer'Class(Object), Output, Object.Configuration.Background_Image_File_Name.all);
		elsif Tag = "back" then
			Write(Output, "<a href=");
			Link(Renderer'Class(Object), Output, User_Id => User_Id, User_Password => User_Password);
			Write(Output, '>');
			Web.Producers.Produce(Output, Template);
			Write(Output, "</a>");
		else
			raise Program_Error with "Invalid template """ & Tag & """";
		end if;
	end Handle_List;
	
	procedure Handle_Users(
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		Tag : in String;
		Template : in Web.Producers.Template;
		Object : in Renderer;
		Village_Id : in Villages.Village_Id := Villages.Invalid_Village_Id;
		User_Id : in String;
		User_Password : in String) is
	begin
		if Tag = "id" then
			Write(Output, '"');
			Write(Output, User_Id);
			Write(Output, '"');
		elsif Tag = "password" then
			Write(Output, '"');
			Web.Write_In_Attribute (Output, Renderer'Class(Object).HTML_Version, User_Password);
			Write(Output, '"');
		elsif Tag = "uri" then
			Link(Renderer'Class(Object), Output, Village_Id => Village_Id,
				User_Id => User_Id, User_Password => User_Password);
		elsif Tag = "villageid" then
			if Village_Id = Villages.Invalid_Village_Id then
				Write(Output, """""");
			else
				Write(Output, '"');
				Write(Output, Village_Id);
				Write(Output, '"');
			end if;
		elsif Tag = "invillage" then
			declare
				procedure Handle(Output : not null access Ada.Streams.Root_Stream_Type'Class;
					Tag : in String; Template : in Web.Producers.Template) is
				begin
					Handle_Users(Output, Tag, Template, Object, Village_Id, User_Id, User_Password);
				end Handle;
			begin
				if Village_Id /= Villages.Invalid_Village_Id then
					Web.Producers.Produce(Output, Template, Handler => Handle'Access);
				end if;
			end;
		else
			raise Program_Error with "Invalid template """ & Tag & """";
		end if;
	end Handle_Users;
	
	procedure Handle_Villages(
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		Tag : in String;
		Template : in Web.Producers.Template;
		Object : in Renderer;
		Village_Id : in Villages.Village_Id;
		Village : in Vampires.Villages.Village_Type;
		Day : in Natural;
		User_Id : in String;
		User_Password : in String) is
	begin
		if Tag = "title" then
			Web.Write_In_HTML (Output, Renderer'Class(Object).HTML_Version, +Village.Name);
			Write(Output, ' ');
			Day_Name(Renderer'Class(Object), Output, Day, Village.Today, Village.State);
			if Template.Is_Empty then
				Write(Output, " - ");
			end if;
		else
			Handle_Users(Output, Tag, Template, Object,
				Village_Id => Village_Id, User_Id => User_Id, User_Password => User_Password);
		end if;
	end Handle_Villages;
	
	procedure Handle_Messages(
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		Tag : in String;
		Template : in Web.Producers.Template;
		Object : in Renderer;
		Village_Id : in Villages.Village_Id;
		Village : in Vampires.Villages.Village_Type;
		Day : in Natural;
		Message : in Vampires.Villages.Message;
		Time : in Ada.Calendar.Time;
		User_Id : in String;
		User_Password : in String)
	is
		People : access constant Vampires.Villages.People.Vector;
		Subject : Natural;
	begin
		case Message.Kind is
			when Vampires.Villages.Escaped_Speech =>
				People := Village.Escaped_People'Access;
				Subject := Message.Subject;
			when Vampires.Villages.Detective_Message_Kind =>
				People := Village.People'Access;
				Subject := Message.Target;
			when others =>
				People := Village.People'Access;
				Subject := Message.Subject;
		end case;
		declare
			Person : Vampires.Villages.Person_Type renames People.Constant_Reference(Subject).Element.all;
		begin
			if Tag = "image" then
				Link_Image(Renderer'Class(Object), Output, +Person.Image);
			elsif Tag = "name" then
				Write(Output, Name(Person));
			elsif Tag = "time" then
				Write(Output, Ada.Calendar.Formatting.Image(Time, Time_Zone => Calendar.Time_Offset));
			elsif Tag = "text" then
				declare
					S : String renames Ada.Strings.Unbounded.To_String(Message.Text);
				begin
					if S'Length > Max_Length_Of_Message then
						declare
							I : Natural := Max_Length_Of_Message;
						begin
							while Character'Pos (S (I + 1)) in 16#80# .. 16#bf# loop
								I := I - 1;
							end loop;
							Web.Write_In_HTML (Output, Renderer'Class(Object).HTML_Version, S(1 .. I));
							Write(Output, "<b>");
							Web.Write_In_HTML (Output, Renderer'Class(Object).HTML_Version, S(I + 1 .. S'Last));
							Write(Output, "</b>");
						end;
					else
						Web.Write_In_HTML (Output, Renderer'Class(Object).HTML_Version, S);
					end if;
				end;
			else
				Handle_Villages(Output, Tag, Template, Object,
					Village_Id, Village, Day, User_Id => User_Id, User_Password => User_Password);
			end if;
		end;
	end Handle_Messages;
	
	function Get_Village_Id(
		Object : Renderer;
		Query_Strings : Web.Query_Strings) return Villages.Village_Id
	is
		S : String renames Web.Element(Query_Strings, "village");
	begin
		if S'Length = Villages.Village_Id'Length then
			return S;
		else
			return Villages.Invalid_Village_Id;
		end if;
	end Get_Village_Id;
	
	procedure Get_Day(
		Object : in Renderer;
		Village : in Vampires.Villages.Village_Type;
		Query_Strings : in Web.Query_Strings;
		Day : out Natural)
	is
		S : String renames Web.Element(Query_Strings, "day");
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
	
	procedure Get_Range(
		Object : in Renderer;
		Village : in Vampires.Villages.Village_Type;
		Day : in Natural;
		Query_Strings : in Web.Query_Strings;
		First, Last : out Integer)
	is
		function Escaped (
			Village : Vampires.Villages.Village_Type;
			Message : Vampires.Villages.Message) return Boolean is
		begin
			case Message.Kind is
				when Vampires.Villages.Introduction
					| Vampires.Villages.Narration =>
					return True;
				when Vampires.Villages.Escaped_Join
					| Vampires.Villages.Escaped_Speech
					| Vampires.Villages.Escape =>
					return Vampires.Villages.Rejoined (
						Village, Message.Subject) < 0;
				when others =>
					return False;
			end case;
		end Escaped;
	begin
		if Village.State = Villages.Prologue and then
			Day = 0 and then
			Web.Element (Query_Strings, "range") = ""
		then
			declare
				Index : Natural := 0;
			begin
				First := 0;
				Last := -1;
				while Index <= Village.Messages.Last_Index and then Escaped (
					Village,
					Village.Messages.Constant_Reference (Index).Element.all)
				loop
					case Village.Messages.Constant_Reference (Index).Element.Kind
					is
						when Vampires.Villages.Speech
							| Vampires.Villages.Escaped_Speech =>
							First := First + 1;
						when others =>
							null;
					end case;
					Index := Index + 1;
				end loop;
			end;
		else
			First := -1;
			Last := -1;
		end if;
	end Get_Range;
	
	function Get_User_Id(
		Object : Renderer;
		Query_Strings : Web.Query_Strings;
		Cookie : Web.Cookie) return String is
	begin
		return Web.Element(Cookie, "id");
	end Get_User_Id;
	
	function Get_User_Password(
		Object : Renderer;
		Query_Strings : Web.Query_Strings;
		Cookie : Web.Cookie) return String is
	begin
		return Web.Element(Cookie, "password");
	end Get_User_Password;
	
	procedure Set_User(
		Object : in Renderer;
		Cookie : in out Web.Cookie;
		New_User_Id: in String;
		New_User_Password : in String) is
	begin
		Web.String_Maps.Include(Cookie, "id", New_User_Id);
		Web.String_Maps.Include(Cookie, "password", New_User_Password);
	end Set_User;
	
	function Get_Text(
		Object : Renderer;
		Inputs : Web.Query_Strings) return String is
	begin
		return Ada.Strings.Fixed.Trim(Web.Element(Inputs, "text"), Ada.Strings.Both);
	end Get_Text;
	
	function Is_User_Page(
		Object : Renderer;
		Query_Strings : Web.Query_Strings;
		Cookie : Web.Cookie) return Boolean
	is
		User_Id : String renames Web.Element(Cookie, "id");
	begin
		if User_Id = "" then
			return False;
		else
			return Web.Element(Query_Strings, "user") = User_Id;
		end if;
	end Is_User_Page;
	
	-- Page Generating
	
	procedure Refresh_Page(
		Object : in Renderer;
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		URI : in String) is
	begin
		Write(Output,
			"<meta http-equiv=""REFRESH"" content=""0;URL=" & URI & """ />" &
			"<style>body{background-color:black;}</style>");
	end Refresh_Page;
	
	procedure Produce(
		Object : in Renderer;
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		File_Name : in String;
		Handler : not null access procedure(Output : not null access Ada.Streams.Root_Stream_Type'Class; Tag : in String; Contents : Web.Producers.Template))
	is
		File: Ada.Streams.Stream_IO.File_Type;
	begin
		Ada.Streams.Stream_IO.Open(File, Ada.Streams.Stream_IO.In_File, File_Name);
		Web.Producers.Produce(Output,
			Web.Producers.Read(Ada.Streams.Stream_IO.Stream(File), Natural(Ada.Streams.Stream_IO.Size(File))),
			Handler => Handler);
		Ada.Streams.Stream_IO.Close(File);
	end Produce;
	
	procedure Link_Style_Sheet(
		Object : in Renderer;
		Output : not null access Ada.Streams.Root_Stream_Type'Class) is
	begin
		Write(Output, '"');
		Write(Output, Object.Configuration.Style_Sheet_File_Name.all);
		Write(Output, '"');
	end Link_Style_Sheet;

	procedure User_Panel(
		Object : in Renderer;
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		Template : in Web.Producers.Template;
		User_Id : in String;
		User_Password : in String;
		Link_To_User_Page : Boolean)
	is
		procedure Handle(Output : not null access Ada.Streams.Root_Stream_Type'Class;
			Tag : in String; Template : in Web.Producers.Template) is
		begin
			if Tag = "id" then
				if not Link_To_User_Page or else User_Id = Tabula.Users.Administrator then
					Write(Output, User_Id);
				else
					Write(Output, "<a href=");
					Link(Renderer'Class(Object), Output,
						User_Id => User_Id, User_Password => User_Password, User_Page => True);
					Write(Output, '>');
					Write(Output, User_Id);
					Write(Output, "</a>");
				end if;
			elsif Tag = "administrator" then
				if User_Id = Tabula.Users.Administrator then
					Web.Producers.Produce(Output, Template, Handler => Handle'Access);
				end if;
			else
				Handle_Users(Output, Tag, Template, Object,
					User_Id => User_Id, User_Password => User_Password);
			end if;
		end Handle;
		Extract : constant array(Boolean) of access constant String := (
			new String'("logoff"), new String'("logon"));
	begin
		Web.Producers.Produce(Output, Template, Extract(User_Id /= "").all, Handler => Handle'Access);
	end User_Panel;
	
	procedure Link(
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
		User_Page : Boolean := False) is
	begin
		Write(Output, '"');
		if User_Page then
			Write(Output, "?user=");
			Write(Output, User_Id);
		elsif Log then
			Write(Output, "./");
			Write(Output, Object.Configuration.Log_Directory.all);
			Write(Output, '/');
			Write(Output, Village_Id);
			Write(Output, "-0.html");
		else
			Write(Output, '?');
			if Village_Id /= Villages.Invalid_Village_Id then
				Write(Output, "village=");
				Write(Output, Village_Id);
				if Day >= 0 then
					Write(Output, "&day=");
					Write(Output, To_String(Day));
				end if;
				if Day = 0 and then First = 0 then
					Write (Output, "&range=all");
				end if;
			end if;
		end if;
		Write(Output, '"');
	end Link;
	
	procedure Link_Image(
		Object : in Renderer;
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		File_Name : in String) is
	begin
		Write(Output, '"');
		Write(Output, Object.Configuration.Image_Directory.all);
		Write(Output, "/");
		Write(Output, File_Name);
		Write(Output, '"');
	end Link_Image;
	
	procedure Day_Name(
		Object : in Renderer;
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		Day : Natural;
		Today : Natural;
		State : Villages.Village_State) is
	begin
		if Day = 0 then
			Write(Output, "プロローグ");
		elsif (State >= Villages.Epilogue) and (Today = Day) then
			Write(Output, "エピローグ");
		else
			Write(Output, Natural'Image(Day) & "日目");
		end if;
	end Day_Name;
	
	function HTML_Version(Object : in Renderer) return Web.HTML_Version is
	begin
		return Web.XHTML;
	end HTML_Version;
	
end Tabula.Renderers;
