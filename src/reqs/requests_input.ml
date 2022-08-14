open Data_types
let user_test1 =
  {
    username = "test_user0";
    email = "azwbdklo@gmail.com";
    password = "dummy1234!";
    user_desc = "This user is here for testing purposes";
    first_login_date = "25-04-1997 20:45:30";
  }

let user_test2 =
  {
    username = "test_user1";
    email = "azwbdj@hotmail.com";
    password = "dummy1234!";
    user_desc = "This user is also here for testing purposes";
    first_login_date = "25-04-1997 20:45:30";
  }

let user_test3 =
  {
    username = "test_user3";
    email = "example@gmail.com";
    password = "dummydedada1234!";
    user_desc = "This user is also here for testing purposes";
    first_login_date = "25-04-1997 20:45:30";
  }

let test_user4 =
  {
    username = "test_user2";
    email = "elias.ben@gmail.com";
    password = "Testing123%!";
    user_desc = "User added from postman";
    first_login_date = "irrelevant";
  }

let user_test_tyler =
  {
    username = "tyler";
    email = "tyler_durden@gmail.com";
    password = "2ndRulerefRule1$!";
    user_desc = "3rd user for testing only";
    first_login_date = "2022-08-07 14:45:52.523274";
  }

let user_test_marla =
  {
    username = "marla";
    email = "marla1991@hotmail.fr";
    password = "Rule1$!%";
    user_desc = "Here for testing only";
    first_login_date = "2022-08-07 14:45:52.523274";
  }

let user_test_james =
  {
    username = "james";
    email = "james.dean@gmail.com";
    password = "examPlePass1!";
    user_desc = "test user 1";
    first_login_date = "2022-08-07 14:45:52.523274";
  }

let default_users_list = [ user_test1; user_test2; user_test3 ]

let jobs =
  [
    {
      job_client = "ocamlpro";
      job_ref_tag = 1;
      order_ts = "2022-08-10 18:24:22";
      path_to_f = "root";
      priority = 100;
      status = "scheduled";
    };
    {
      job_client = "ocamlpro";
      job_ref_tag = 2;
      order_ts = "2022-08-10 18:24:22";
      path_to_f = "root";
      priority = 200;
      status = "scheduled";
    };
    {
      job_client = "ocamlpro";
      job_ref_tag = 3;
      order_ts = "2022-08-10 18:24:22";
      path_to_f = "root";
      priority = 300;
      status = "scheduled";
    };
  ]