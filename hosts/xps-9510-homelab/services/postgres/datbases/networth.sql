DO
$do$
BEGIN
create table if not exists accounts (
    id serial primary key,
    name text not null unique,
    notes text default '',
    align decimal(12, 2) default 0
);

create table if not exists expense_accounts (
    id serial primary key unique,
    name text not null,
    notes text default ''
);

create table if not exists income_accounts (
    id serial primary key,
    name text not null unique,
    notes text default ''
);

create table if not exists expense_categories (
    id serial primary key,
    name text not null unique,
    notes text default ''
);

create table if not exists income_categories (
    id serial primary key,
    name text not null unique,
    notes text default ''
);

create table if not exists transfer_categories (
    id serial primary key,
    name text not null unique,
    notes text default ''
);

create table if not exists expenses (
    id serial primary key,
    amount decimal(12, 2) not null,
    description text default '',
    date date not null,
    source serial,
    destination serial,
    category serial,
    constraint fk_source foreign key (source) references accounts(id),
    constraint fk_destination foreign key (destination) references expense_accounts(id),
    constraint fk_category foreign key (category) references expense_categories(id)
);

create table if not exists incomes (
    id serial primary key,
    amount decimal(12, 2) not null,
    description text default '',
    date date not null,
    source serial,
    destination serial,
    category serial,
    constraint fk_source foreign key (source) references income_accounts(id),
    constraint fk_destination foreign key (destination) references accounts(id),
    constraint fk_category foreign key (category) references income_categories(id)
);

create table if not exists transfers (
    id serial primary key,
    amount decimal(12, 2) not null,
    description text default '',
    date date not null,
    source serial,
    destination serial,
    category serial,
    constraint fk_source foreign key (source) references accounts(id),
    constraint fk_destination foreign key (destination) references accounts(id),
    constraint fk_category foreign key (category) references transfer_categories(id)
);

create table if not exists budgets (
    id serial primary key,
    amount decimal(12, 2) not null,
    category serial,
    constraint fk_category foreign key (category) references expense_categories(id)
);

create or replace view
    net_worth
as
select i.name, i.sum - e.sum + a.align + t.sum as balance
from (
        select a.name, coalesce(sum(e.amount), 0) as sum
        from expenses as e
        full outer join accounts as a
        on e.source = a.id
        group by a.name
     ) as e,
     (
        select a.name, coalesce(sum(i.amount), 0) as sum
        from incomes as i
        full outer join accounts as a
        on i.destination = a.id
        group by a.name
     ) as i,
     (
        select i.name, i.sum - o.sum as sum
        from (
                select a.name, coalesce(sum(t.amount), 0) as sum
                from transfers as t
                full outer join accounts as a
                on t.destination = a.id
                group by a.name
            ) as i,
            (
                select a.name, coalesce(sum(t.amount), 0) as sum
                from transfers as t
                full outer join accounts as a
                on t.source = a.id
                group by a.name
            ) as o
        where i.name = o.name
     ) as t,
    accounts as a
where i.name = e.name and a.name = e.name and t.name = a.name;

ALTER TABLE accounts OWNER TO networth;
ALTER TABLE expenses OWNER TO networth;
ALTER TABLE expense_accounts OWNER TO networth;
ALTER TABLE expense_categories OWNER TO networth;
ALTER TABLE income_accounts OWNER TO networth;
ALTER TABLE incomes OWNER TO networth;
ALTER TABLE income_categories OWNER TO networth;
ALTER TABLE transfers OWNER TO networth;
ALTER TABLE transfer_categories OWNER TO networth;
ALTER TABLE budgets OWNER TO networth;
ALTER VIEW  net_worth OWNER to networth;
END
$do$;
