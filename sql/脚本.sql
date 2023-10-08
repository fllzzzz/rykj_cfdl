ALTER  TABLE  user_verify  DROP  PRIMARY  KEY;
ALTER  TABLE  user_verify
add COLUMN id bigint   comment 'id',
add COLUMN user_name    varchar(64)    comment '申请人',
add COLUMN vehicle_img           mediumtext             comment '车辆照片',
add PRIMARY key(id);

drop table if exists lottery_black_list;
create table lottery_black_list(
  id           bigint(0)            not null           comment 'id',
  user_id      bigint(0)            default 0          comment 'userId',
  job_number   varchar(64)          default ''         comment '工号',
  name         varchar(64)          default ''         comment '姓名',
  reason      varchar(128)          default ''         comment '原因',
  create_tm        timestamp(3)                        comment '创建时间',
  update_tm        timestamp(3)                        comment '更新时间',
  primary key (id) USING BTREE,
  UNIQUE INDEX `udx_user_id`(`user_id`) USING BTREE
)engine=innodb  comment ='摇号黑名单表' ROW_FORMAT = Dynamic;


drop table if exists parking_lot;
create table parking_lot(
  id           bigint(0)            not null           comment 'id',
  parent_id    bigint(0)            default 0          comment '上级id',
  region      varchar(64)           default ''         comment '区域（园区名称、停车场名称）',
  region_code varchar(512)           default ''         comment '区域编号（园区的话区域编号为json数组，停车场的话为单个闸机的编号）',
  amount        int(0)              default 0          comment '车位数量',
  type          char(1)             default 0         comment '类型(0：可摇号，1：不可摇号)',
  remark      varchar(128)          default ''         comment '备注',
  image_info      longtext                            comment '图片信息',
  create_tm     timestamp(3)                          comment '创建时间',
  update_tm     timestamp(3)                          comment '更新时间',
  primary key (id) USING BTREE,
  UNIQUE INDEX `udx_region_code`(`region_code`) USING BTREE
)engine=innodb  comment ='停车场表' ROW_FORMAT = Dynamic;



drop table if exists lottery_rule_round;
create table lottery_rule_round(
  id           bigint(0)            not null           comment 'id',
  name         varchar(32)          default ''         comment '轮数名称',
  parking_lot_code varchar(256)     default ''         comment '停车场(编号)，多个间逗号间隔',
  state        char(1)              default 0          comment '状态(0：停用，1：启用)',
  remark       varchar(128)         default ''         comment '备注',
  create_tm    timestamp(3)                            comment '创建时间',
  update_tm    timestamp(3)                            comment '更新时间',
  primary key (id) USING BTREE
)engine=innodb  comment ='摇号规则-轮数表' ROW_FORMAT = Dynamic;



drop table if exists lottery_rule_assign;
create table lottery_rule_assign(
  id           bigint(0)            not null           comment 'id',
  type         varchar(32)          default ''         comment '分配类型（1：按部门分配；2：按人员分配）',
  code         varchar(16)          default ''         comment '部门编码/人员工号',
  name         varchar(32)          default ''         comment '名称（部门或者人员名称）',
  parking_lot_code  varchar(256)    default ''         comment '停车场(编号)，多个间逗号间隔',
  state        char(1)              default 0          comment '状态(0：停用，1：启用)',
  remark       varchar(128)         default ''         comment '备注',
  create_tm    timestamp(3)                            comment '创建时间',
  update_tm    timestamp(3)                            comment '更新时间',
  primary key (id) USING BTREE
)engine=innodb  comment ='摇号规则-停车场分配表' ROW_FORMAT = Dynamic;



drop table if exists lottery_batch;
create table lottery_batch(
  id           bigint(0)            not null           comment 'id',
  batch_num      date                      comment '期号',
  parking_amount int(0)             default 0          comment '车位数量',
  round_id       varchar(512)        default ''         comment '摇号轮数，多个间逗号间隔',
  apply_start_time     datetime                        comment '报名开始时间',
  apply_end_time       datetime                        comment '报名结束时间',
  valid_start_date     date                            comment '车位有效开始日期',
  valid_end_date       date                            comment '车位有效截止日期',
  state        char(1)              default 0          comment '状态（0：待通知；1：已通知；2：已结束）',
  remark       varchar(128)         default ''         comment '备注',
  create_tm    timestamp(3)                            comment '创建时间',
  update_tm    timestamp(3)                            comment '更新时间',
  primary key (id) USING BTREE
)engine=innodb  comment ='摇号批次表' ROW_FORMAT = Dynamic;



drop table if exists lottery_result;
create table lottery_result(
  id           bigint(0)            not null           comment 'id',
  batch_id     bigint(0)            default 0          comment '摇号批次id',
  batch_num      date                      comment '期号',
  round_id     bigint(0)            default 0          comment '轮数',
  state        char(1)              default 0          comment '状态（0：待摇号；1：待确认；2：确认中；3：待发布；4：待归档；5：已归档）',
  create_tm    timestamp(3)                            comment '创建时间',
  update_tm    timestamp(3)                            comment '更新时间',
  primary key (id) USING BTREE
)engine=innodb  comment ='摇号结果表' ROW_FORMAT = Dynamic;



drop table if exists lottery_result_detail;
create table lottery_result_detail(
  id           bigint(0)            not null           comment 'id',
  result_id    bigint(0)            default 0          comment '摇号结果表id',
  parking_lot_code  varchar(32)     default ''         comment '停车场编号',
  user_id      bigint(0)            default 0          comment '用户id',
  user_name     varchar(64)         default ''        comment '用户姓名',
  user_job_number   varchar(128)    default ''        comment '用户工号',
  state        char(1)              default 0          comment '状态（0：待摇号；1：待确认；2：待发布；3：待归档）',
  create_tm    timestamp(3)                            comment '创建时间',
  update_tm    timestamp(3)                            comment '更新时间',
  primary key (id) USING BTREE
)engine=innodb  comment ='摇号结果详情表' ROW_FORMAT = Dynamic;



drop table if exists lottery_apply_record;
create table lottery_apply_record(
  id           bigint(0)            not null           comment 'id',
  batch_id     bigint(0)            default 0          comment '摇号批次id',
  batch_num      date                      comment '期号',
  parking_lot_code     varchar(256)  default ''         comment '停车场编号',
  valid_start_date     date                            comment '车位有效开始日期',
  valid_end_date       date                            comment '车位有效截止日期',
  user_id      bigint(0)            default 0          comment '申请人id',
  user_name     varchar(64)         default ''        comment '申请人姓名',
  job_number     varchar(128)          default ''         comment '申请人工号',
  apply_state  char(1)              default 0          comment '申请状态(0：取消申请；1：申请)',
  result       varchar(16)          default ''         comment '摇号结果(-1：未开号；0：未中；xx：对应停车场的区域编号)',
  create_tm    timestamp(3)                            comment '创建时间',
  update_tm    timestamp(3)                            comment '更新时间',
  primary key (id) USING BTREE
)engine=innodb  comment ='摇号申请记录表' ROW_FORMAT = Dynamic;





drop table if exists parking_space_transfer_record;
create table parking_space_transfer_record(
  id           bigint(0)            not null           comment 'id',
  parking_lot_code  varchar(32)     default ''         comment '转赠停车场编码',
  user_id         bigint(0)         default 0          comment '申请人userId',
  accept_user_id  bigint(0)         default 0          comment '赠予人userId',
  accept_user_name   varchar(64)    default ''         comment '赠予人姓名',
  valid_start_date     date                            comment '转赠有效开始日期',
  valid_end_date       date                            comment '转赠有效截止日期',
  create_tm    timestamp(3)                            comment '创建时间',
  update_tm    timestamp(3)                            comment '更新时间',
  primary key (id) USING BTREE
)engine=innodb  comment ='车位转赠记录表' ROW_FORMAT = Dynamic;


alter table lottery_apply_record add index batch_id_idx(batch_id);

alter table lottery_apply_record add column parking_lot_code varchar(256) default '' comment '停车场编号';
alter table lottery_rule_assign add index code_idx(code,type);

alter table user_space add schedule_date varchar(10) default '' comment '定时器执行时间';
alter table user_space add column batch_num date comment '期号',add column round_id bigint default 0 comment '摇号轮数id';
alter table user_space  add column batch_id BIGINT comment '批次ID';

alter table user_profile  add column parking_lot_region varchar(64) default '' comment '停车场区域';
alter table lottery_result add index batch_id_idx(batch_id);
alter table lottery_result_detail add index result_id_idx(result_id);
alter table lottery_result add index state_idx(state);
alter table user_space add index job_number_idx(job_number,parking_lot);
alter table user_space add index batch_id_idx(batch_id,round_id);
alter table user_space add index schedule_date_idx(schedule_date);

alter table user_space add column  fail_reason varchar(256) default '' comment '失败原因';

alter table lottery_apply_record add index job_number_idx (job_number);
alter table user_profile add index job_number_idx (job_number);

alter table lottery_batch add unique index batch_num_idx (batch_num);


-- 20230924 
alter table lottery_rule_assign add column round_id bigint comment '轮次id';
alter table lottery_rule_assign add index round_idx(round_id);
alter table user_space add column  type char(1) default '0' comment '类型，1摇号，2默认';

--20230927
alter table lottery_rule_assign add column round_name varchar(32) default ''  comment '轮数名称';
alter table lottery_rule_assign add column  parking_lot_region  varchar(64) default '' comment '停车场区域';
alter table lottery_rule_assign MODIFY column parking_lot_code  varchar(64) default '' comment '停车场编号';
alter table lottery_rule_assign MODIFY COLUMN code VARCHAR(2048) default '' comment '部门编码/人员工号（可多选）';
alter table lottery_rule_assign MODIFY COLUMN name VARCHAR(2048) default '' comment '名称（部门或者人员名称）';

--20231008
alter table lottery_apply_record MODIFY column result varchar(16) default '' comment '摇号结果(-1：未开号；0：未中；1：摇中)';
