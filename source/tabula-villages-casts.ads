-- The Village of Vampire by YT, このソースコードはNYSLです
package Tabula.Villages.Casts is
	
	type Work is record
		Name : Ada.Strings.Unbounded.Unbounded_String;
		Sex : Sex_Kind;
		Nominated : Boolean;
	end record;
	
	Default_Work : constant Work := (
		Name => Ada.Strings.Unbounded.Null_Unbounded_String,
		Sex => Neutral,
		Nominated => False);
	
	type Work_Array is array(Natural range <>) of Work;
	type Work_Array_Access is access Work_Array;
	procedure Free is new Ada.Unchecked_Deallocation(Work_Array, Work_Array_Access);
	
	type Cast_Type is new Ada.Finalization.Limited_Controlled with record
		People : Person_Array_Access;
		Works : Work_Array_Access;
	end record;
	overriding procedure Finalize(Object : in out Cast_Type);
	pragma Finalize_Storage_Only(Cast_Type);
	
	procedure Exclude_Taken(Cast : in out Casts.Cast_Type; Village : Village_Type);
	
end Tabula.Villages.Casts;
