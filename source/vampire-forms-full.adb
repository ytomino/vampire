-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Strings.Fixed;
package body Vampire.Forms.Full is
	use type Tabula.Villages.Village_State;
	
	function Create return Form_Type is
	begin
		return (null record);
	end Create;
	
	overriding function Get_User_Id (
		Form : Form_Type;
		Query_Strings : Web.Query_Strings;
		Cookie : Web.Cookie)
		return String is
	begin
		return Web.Element (Cookie, "id");
	end Get_User_Id;
	
	overriding function Get_User_Password (
		Form : Form_Type;
		Query_Strings : Web.Query_Strings;
		Cookie : Web.Cookie)
		return String is
	begin
		return Web.Element (Cookie, "password");
	end Get_User_Password;
	
	overriding procedure Set_User (
		Form : in out Form_Type;
		Cookie : in out Web.Cookie;
		New_User_Id: in String;
		New_User_Password : in String) is
	begin
		null;
	end Set_User;
	
	overriding function Is_User_Page (
		Form : Form_Type;
		Query_Strings : Web.Query_Strings;
		Cookie : Web.Cookie)
		return Boolean
	is
		User_Id : constant String := Form.Get_User_Id (Query_Strings, Cookie);
	begin
		if User_Id'Length = 0 then
			return False;
		else
			return Web.Element (Query_Strings, "user") = User_Id;
		end if;
	end Is_User_Page;
	
	overriding function Get_Village_Id (
		Form : Form_Type;
		Query_Strings : Web.Query_Strings)
		return Tabula.Villages.Village_Id
	is
		S : constant String := Web.Element (Query_Strings, "village");
	begin
		if S'Length = Tabula.Villages.Village_Id'Length then
			return S;
		else
			return Tabula.Villages.Invalid_Village_Id;
		end if;
	end Get_Village_Id;
	
	overriding function Get_Day (
		Form : Form_Type;
		Village : Villages.Village_Type; 
		Query_Strings : Web.Query_Strings)
		return Natural
	is
		S : constant String := Web.Element (Query_Strings, "day");
	begin
		return Natural'Value (S);
	exception
		when Constraint_Error =>
			if Village.State /= Tabula.Villages.Closed then
				return Village.Today;
			else
				return 0;
			end if;
	end Get_Day;
	
	overriding function Get_Range (
		Form : Form_Type;
		Village : Villages.Village_Type; 
		Day : Natural;
		Query_Strings : Web.Query_Strings)
		return Message_Range
	is
		function Escaped (
			Village : Villages.Village_Type;
			Message : Villages.Message) return Boolean is
		begin
			case Message.Kind is
				when Villages.Introduction
					| Villages.Narration =>
					return True;
				when Villages.Escaped_Join
					| Villages.Escaped_Speech
					| Villages.Escape =>
					declare
						Rejoined : Tabula.Villages.Person_Index'Base := Village.Joined (
							Village.Escaped_People.Constant_Reference (Message.Subject).Element.
								Id.Constant_Reference.Element.all);
					begin
						return Rejoined = Tabula.Villages.No_Person
							or else not Tabula.Villages.Same_Id_And_Figure (
								Village.Escaped_People.Constant_Reference (Message.Subject).Element.all,
								Village.People.Constant_Reference (Rejoined).Element.all);
					end;
				when others =>
					return False;
			end case;
		end Escaped;
	begin
		if Village.State = Tabula.Villages.Prologue
			and then Day = 0
			and then Web.Element (Query_Strings, "range") = ""
		then
			declare
				First : Integer := 0;
				Last : Integer := -1;
				Index : Natural := 0;
			begin
				while Index <= Village.Messages.Last_Index
					and then Escaped (
						Village,
						Village.Messages.Constant_Reference (Index).Element.all)
				loop
					case Village.Messages.Constant_Reference (Index).Element.Kind is
						when Villages.Speech
							| Villages.Escaped_Speech =>
							First := First + 1;
						when others =>
							null;
					end case;
					Index := Index + 1;
				end loop;
				return (First, Last);
			end;
		else
			return (-1, -1);
		end if;
	end Get_Range;
	
	overriding function Get_Text (
		Form : Form_Type;
		Inputs : Web.Query_Strings)
		return String is
	begin
		return Ada.Strings.Fixed.Trim (Web.Element (Inputs, "text"), Ada.Strings.Both);
	end Get_Text;
	
end Vampire.Forms.Full;
