-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Numerics.MT19937;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Text_IO.Text_Streams;
with Web;
with Tabula;
with Tabula.Calendar;
with Tabula.Villages;
with Tabula.Villages.Shuffle;
use Tabula;
use Tabula.Villages.People;
use Tabula.Villages.Person_Records;
procedure Shuffle is
	Seed : aliased Ada.Numerics.MT19937.Generator;
	subtype People_Count is Integer range Minimum_Number_Of_Persons .. Maximum_Number_Of_Persons;
	package Random_People_Count is new Ada.Numerics.MT19937.Discrete_Random(People_Count);
	package Random_Teaming is new Ada.Numerics.MT19937.Discrete_Random(Villages.Teaming);
	Village : Villages.Village_Type := (
		Name => Ada.Strings.Unbounded.Null_Unbounded_String,
		By => Ada.Strings.Unbounded.Null_Unbounded_String,
		State => Villages.Prologue,
		Today => 0,
		Time => Villages.Daytime,
		Dawn => Calendar.Null_Time,
		Day_Duration => 0.0,
		Night_Duration => 0.0,
		Victim_Existing      => Villages.Initial_Victim_Existing, Victim_Role => Villages.Inhabitant,
		Teaming              => Villages.Initial_Teaming,
		Monster_Side         => Villages.Initial_Monster_Side,
		Attack               => Villages.Initial_Attack,
		Servant_Knowing      => Villages.Initial_Servant_Knowing,
		Daytime_Preview      => Villages.Initial_Daytime_Preview,
		Doctor_Infected      => Villages.Initial_Doctor_Infected,
		Hunter_Silver_Bullet => Villages.Initial_Hunter_Silver_Bullet,
		Unfortunate          => Villages.Initial_Unfortunate,
		Appearance => (others => Villages.Random),
		People => Empty_Vector,
		Escaped_People => Empty_Vector,
		Messages => Villages.Messages.Empty_Vector);
	Output : Ada.Text_IO.Text_Streams.Stream_Access := Ada.Text_IO.Text_Streams.Stream(Ada.Text_IO.Standard_Output);
begin
	Ada.Numerics.MT19937.Reset(Seed);
	Village.Teaming := Random_Teaming.Random(Seed'Access);
	Web.Header(Output, Web.Text);
	String'Write(Output, Web.Line_Break);
	for I in 1 .. Random_People_Count.Random(Seed'Access) loop
		Append(Village.People, Villages.Person_Type'(
			Name => Ada.Strings.Unbounded.To_Unbounded_String("" & Character'Val(Character'Pos(Character'Pred('A')) + I)),
			Image => Ada.Strings.Unbounded.Null_Unbounded_String,
			Sex => Villages.Sex_Kind'Val((I rem 2) * Villages.Sex_Kind'Pos(Villages.Male) + (1 - I rem 2) * Villages.Sex_Kind'Pos(Villages.Female)),
			Group => 0,
			Work => Ada.Strings.Unbounded.Null_Unbounded_String,
			Request => Villages.Random,
			Ignore_Request => False,
			Role => Villages.Inhabitant, 
			Id => Ada.Strings.Unbounded.Null_Unbounded_String,
			Commited => False,
			Records => Empty_Vector));
	end loop;
	Tabula.Villages.Shuffle(Village.People, null, Village.Teaming, Village.Monster_Side, Village.Appearance, Seed'Access);
	String'Write(Output, Villages.Teaming'Image(Village.Teaming));
	String'Write(Output, Web.Line_Break);
	String'Write(Output, Web.Line_Break);
	for I in Village.People.First_Index .. Village.People.Last_Index loop
		String'Write(Output, Villages.Person_Role'Image(Village.People.Constant_Reference(I).Element.Role));
		String'Write(Output, Web.Line_Break);
	end loop;
end Shuffle;