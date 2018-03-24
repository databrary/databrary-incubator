 select 
     volume
   , min(race) as race
   , min(eth) as eth
   , min(date_part('year',age(cast(dob as date)))) as age 
 from 
   (select record
         , volume
         , case when metric = 6 then datum else null end as race
         , case when metric = 7 then datum e null end as eth
         , case when metric = 4 then datum else null end as dob 
    from record r 
      inner join measure m on r.id = m.record 
    where category = 1
      and metric in (6,7,4)) vs 
 group by record, volume
