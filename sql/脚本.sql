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

-- 20230927
alter table lottery_rule_assign add column round_name varchar(32) default ''  comment '轮数名称';
alter table lottery_rule_assign add column  parking_lot_region  varchar(64) default '' comment '停车场区域';
alter table lottery_rule_assign MODIFY column parking_lot_code  varchar(64) default '' comment '停车场编号';
alter table lottery_rule_assign MODIFY COLUMN code VARCHAR(2048) default '' comment '部门编码/人员工号（可多选）';
alter table lottery_rule_assign MODIFY COLUMN name VARCHAR(2048) default '' comment '名称（部门或者人员名称）';

-- 20231008
alter table lottery_apply_record MODIFY column result varchar(16) default '' comment '摇号结果(-1：未开号；0：未中；1：摇中)';
alter table user_space add column  state char(1) default '0' comment '状态（0：未同步；1：同步成功；2：同步失败）';

-- 20231009
drop table if exists parking_space_change_record;
CREATE TABLE `parking_space_change_record` (
  `id` bigint NOT NULL COMMENT 'id',
  `parking_code` varchar(32) DEFAULT '0' COMMENT '申请人停车场编号',
  `user_id` bigint DEFAULT '0' COMMENT '申请人userId',
	`user_name` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT '' COMMENT '申请人姓名',
  `accept_user_id` bigint DEFAULT '0' COMMENT '交换人userId',
  `accept_user_name` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT '' COMMENT '交换人姓名',
 `accept_parking_code` varchar(32) DEFAULT '0' COMMENT '交换人停车场编号', 
 `valid_start_date` date DEFAULT NULL COMMENT '转赠有效开始日期',
  `valid_end_date` date DEFAULT NULL COMMENT '转赠有效截止日期',
	state char(1) default '0' comment '状态（0：申请，1：已同意，2：已拒绝，3：已撤销）',
  `create_tm` timestamp(3) NULL DEFAULT NULL COMMENT '创建时间',
  `update_tm` timestamp(3) NULL DEFAULT NULL COMMENT '更新时间',
  PRIMARY KEY (`id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci ROW_FORMAT=DYNAMIC COMMENT='车位互换记录表';

drop table if exists lottery_rule_description;
create table lottery_rule_description(
  id           bigint(0)            not null           comment 'id',
  description   varchar(1024)       default ''         comment '规则描述',
  create_tm     timestamp(3)                          comment '创建时间',
  update_tm     timestamp(3)                          comment '更新时间',
  primary key (id) USING BTREE
)engine=innodb  comment ='摇号规则描述表（只有一条记录）' ROW_FORMAT = Dynamic;

ALTER table parking_space_change_record add index user_id_idx(user_id);
ALTER table parking_space_change_record add index acc_user_id_idx(accept_user_id);

alter table parking_lot modify column   `remark` varchar(4000) DEFAULT '' COMMENT '备注';

alter table user_verify MODIFY driving_license_img LONGTEXT default null comment '驾驶证照片';

alter table user_verify MODIFY driving_permit_img LONGTEXT default null comment '行驶证照片';

alter table user_verify MODIFY vehicle_img LONGTEXT default null comment '车辆照片';

alter table `department` add column parent_code varchar(50) not null default '' comment '父级部门code' after dept_code;

-- 10-30
alter table lottery_black_list add type int default 1 comment '类型，1：黑名单，2：领导';


alter table lottery_black_list drop index udx_user_id;

alter table lottery_black_list add unique index udx_user_id(user_id,type);

CREATE TABLE `parking_init` (
  `id` bigint NOT NULL COMMENT 'id',
  `region` varchar(64) DEFAULT '' COMMENT '名称',
  `region_code` varchar(64) DEFAULT '' COMMENT '区域编号',
  `remark` varchar(4000) DEFAULT '' COMMENT '备注',
 `create_tm` timestamp(3) NOT NULL DEFAULT CURRENT_TIMESTAMP(3) COMMENT '创建时间',
  `update_tm` timestamp(3) NOT NULL DEFAULT CURRENT_TIMESTAMP(3) ON UPDATE CURRENT_TIMESTAMP(3) COMMENT '更新时间',
  PRIMARY KEY (`id`) USING BTREE,
  UNIQUE KEY `udx_region_code` (`region_code`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci ROW_FORMAT=DYNAMIC COMMENT='闸机表';

INSERT INTO `parking_init`(`id`, `region`, `region_code`, `remark`, `create_tm`, `update_tm`) VALUES (1, '老园区', '6dc5132a6a7046c09ffd7be54d27ea49', '', '2023-10-30 19:28:12.902', '2023-10-30 19:28:12.902');
INSERT INTO `parking_init`(`id`, `region`, `region_code`, `remark`, `create_tm`, `update_tm`) VALUES (2, '新园区南门', '5a277524d2bc408bbd3097e7ccaf2208', '', '2023-10-30 19:28:45.427', '2023-10-30 19:28:45.427');
INSERT INTO `parking_init`(`id`, `region`, `region_code`, `remark`, `create_tm`, `update_tm`) VALUES (3, '地下停车场', 'aaed725983664c7aa0a1a4dddba3f05c', '', '2023-10-30 19:29:27.734', '2023-10-30 19:29:27.734');
INSERT INTO `parking_init`(`id`, `region`, `region_code`, `remark`, `create_tm`, `update_tm`) VALUES (4, '组装车间1F', '2457999fe2914251976fd333d2816fb2', '', '2023-10-30 19:30:28.375', '2023-10-30 19:30:28.375');
INSERT INTO `parking_init`(`id`, `region`, `region_code`, `remark`, `create_tm`, `update_tm`) VALUES (5, '组装车间4F停车库', '4680b7e1ec414a5ebdf48127f73acd71', '', '2023-10-30 19:31:10.020', '2023-10-30 19:31:10.020');


alter table user_space add column retry_num int default 0 comment '重试次数';

update user_space set state = '1' where state <> '1';