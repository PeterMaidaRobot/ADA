--with Text_IO; use Text_IO;
with
    Ada.Text_IO,
    Ada.Strings.Unbounded,
    Ada.Text_IO.Unbounded_IO,
    Ada.IO_Exceptions;
use
    Ada.Text_IO,
    Ada.Strings.Unbounded,
    Ada.Text_IO.Unbounded_IO,
    Ada.IO_Exceptions;

procedure clue is

    --type Day_Of_Week is (Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday);
    type Turn_Type is (Guess, Approval, Shown); -- a guess is (person, weapon, or room), an approval is (yes, no), a shown is (person, weapon, room, or nothing)
    
    -- the type of cards that a player can have
    type Person_Type is (White, Green, Plum, Mustard);
    package Person_IO is new Ada.Text_IO.Enumeration_IO (Person_Type);
    type Weapon_Type is (Rope, Pipe, Candlestick);
    package Weapon_IO is new Ada.Text_IO.Enumeration_IO (Weapon_Type);
    type Room_Type is (Ballroom, Library, Study);
    package Room_IO is new Ada.Text_IO.Enumeration_IO (Room_Type);
    -- Guess is all of the above


    type Nothing_Type is (Nothing);
    package Nothing_IO is new Ada.Text_IO.Enumeration_IO (Nothing_Type);
    -- Shown is one of the above

    type Approval_Type is (Yes, No);
    package Approval_IO is new Ada.Text_IO.Enumeration_IO (Approval_Type);


    --package Guess_IO is new Ada.Text_IO.Enumeration_IO (Person_Type);
    --type Guess_Type is (Room_Type, Weapon_Type);







-- Letter to Number Hash




---------------------------------------------------------------------------------------

    -- All of the sub-procedures this function uses

---------------------------------------------------------------------------------------


    function Get_Input (Current_Turn : Turn_Type) return Unbounded_String is
        Input_String : Unbounded_String;
        Valid_Input : Boolean := False;
    begin

        -- loop until we get a valid input
        while not Valid_Input loop
            Input_String := To_Unbounded_String(Get_Line);

            -- check if we recieved one of the valid commands
            if Input_String = "Print Guess Table" then 
                Put_Line("Printing Guess Table");
                goto Continue;
            elsif Input_String = "Print List" then 
                Put_Line("Printing List");
                goto Continue;
            elsif Input_String = "Print Log" then 
                Put_Line("Printing Log");
                goto Continue;
            end if;

            -- check if this is the input we were looking for 
            if Current_Turn = Guess then 
                -- check for a person, weapon, and room on a guess turn
                Put_Line("Guess turn");
                Valid_Input := True;
            elsif Current_Turn = Approval then 
                Put_Line("Approval turn");
                -- check for a yes/no on an approval turn
                if Input_String = "yes" or else Input_String = "no" then
                    Valid_Input := True;
                end if;
            elsif Current_Turn = Shown then 
                -- check for a person, weapon, room, or nothing on a shown turn
                Put_Line("Shown turn");
                --if Person_Type(Input_String) in Person_Type then
                    Valid_Input := True;
                --end if;
            end if;

            <<Continue>> -- Label used as a continue statement when looping
        end loop;
                                                                                                                                
        return Input_String;
        -- Returns a valid line
    end get_input;


    procedure play_mom_turn is 
        Input : Unbounded_String;
    begin
        new_line;
        Put_Line("----- START MOM'S TURN -----");

        Put_Line("Enter Mom's guess:");
        Input := Get_Input(Guess);
        

        Put_Line("Did Erin have (yes/no)?");
        Input := Get_Input(Approval);
        

        Put_Line("What did Peter show:");
        Input := Get_Input(Shown);
        
    end play_mom_turn;

    procedure play_erin_turn is 
    begin
        new_line;
        Put_Line("----- START ERIN'S TURN -----");

        Put_Line("Enter Erin's guess:");
        -- check for valid input

        Put_Line("What did Peter show:");
        -- check for valid input

        Put_Line("Did Mom have (yes/no)?");
        -- check for valid input
    end play_erin_turn;

    procedure play_peter_turn is 
    begin
        new_line;
        Put_Line("----- START PETER'S TURN -----");

        Put_Line("Enter Peter's guess:");
        -- check for valid input

        Put_Line("What did Mom show (or nothing):");
        -- check for valid input

        Put_Line("What did Erin show (or nothing):");
        -- check for valid input
    end play_peter_turn;


    procedure get_player_guess is
    begin
        -- Gets the person, weapon, and room.
        Put_Line("null");
    end get_player_guess;

    procedure get_opponent_guess is
    begin
        -- Gets the person, weapon, and room.
        Put_Line("null");
    end get_opponent_guess;

    procedure get_shown is
    begin
        -- Gets the person, weapon, room or nothing.
        Put_Line("null");
    end get_shown;

    -- yes/no



--------------------------------------------------------------------------
S : Unbounded_String;
begin
    -- loop through the three turns forever? or until we find the murderer?
    Put_Line("BEGIN CLUE GAME");

    -- loop forever
    --play_mom_turn;
    --play_erin_turn;
    --play_peter_turn;

    loop
      declare
         Person : Person_Type;
      begin
         Ada.Text_IO.Put("Enter a person: ");
         Person_IO.Get(Person);
         exit;
      exception
         when Ada.IO_Exceptions.Data_Error =>
            Ada.Text_IO.Put_Line("Unrecognized person; try again.");
      end;
   end loop;
    

    Put_Line("END CLUE GAME");
end clue;


-- This clue program assumes:
-- Mom starts, then Erin plays, then Peter plays... Refactor to fix this