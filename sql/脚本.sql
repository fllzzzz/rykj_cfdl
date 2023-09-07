drop table if exists user_verify;
create table user_verify(
  id           bigint(0)            not null           comment 'id',
  user_id      bigint(0)            default 0          comment 'userID',
  user_name    varchar(64)          default ''         comment '申请人',
  plate_no     varchar(20)          default ''         comment '车牌号',
  vehicle_img           mediumtext            comment '车辆照片',
  driving_permit_img    mediumtext            comment '行驶证照片',
  driving_license_img   mediumtext            comment '驾驶证照片',
  state       int(0)                default 0          comment '状态(0:默认，1:待审核，2:审核失败,3:审核成功)',
  reason      varchar(100)          default ''         comment '审核意见',
  create_tm        timestamp(3)                        comment '创建时间',
  update_tm        timestamp(3)                        comment '更新时间',
  primary key (id) USING BTREE
)engine=innodb  comment ='车主认证表（车辆审核表）' ROW_FORMAT = Dynamic;



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
  region      varchar(64)           default ''         comment '区域',
  region_code varchar(32)           default ''         comment '区域编号',
  amount        int(0)              default 0          comment '车位数量',
  type          char(1)             default 0         comment '类型(0：不可摇号，1：可摇号)',
  create_tm     timestamp(3)                          comment '创建时间',
  update_tm     timestamp(3)                          comment '更新时间',
  primary key (id) USING BTREE,
  UNIQUE INDEX `udx_region_code`(`region_code`) USING BTREE
)engine=innodb  comment ='停车场主表' ROW_FORMAT = Dynamic;



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



drop table if exists lottery_rule_assign_type;
create table lottery_rule_assign(
  id           bigint(0)            not null           comment 'id',
  type         varchar(32)          default ''         comment '分配类型（按部门分配、按人员分配）',
  code         varchar(16)          default ''         comment '分配类型编码（section：部门；person：人员）',
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
  round_id       varchar(32)        default ''         comment '摇号轮数，多个间逗号间隔',
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
  round_id     bigint(0)            default 0          comment '轮数',
  state        char(1)              default 0          comment '状态（0：待摇号；1：待确认；2：确认中；3：待发布；4：待归档）',
  create_tm    timestamp(3)                            comment '创建时间',
  update_tm    timestamp(3)                            comment '更新时间',
  primary key (id) USING BTREE
)engine=innodb  comment ='摇号结果表' ROW_FORMAT = Dynamic;



drop table if exists lottery_result_detail;
create table lottery_result_detail(
  id           bigint(0)            not null           comment 'id',
  result_id    bigint(0)            default 0          comment '摇号结果表id',
  parking_lot_id  bigint(0)         default 0          comment '停车场',
  user_id      bigint(0)            default 0          comment '用户',
  plate_no     varchar(12)          default ''         comment '车牌号',
	state        char(1)              default 0          comment '状态（0：未同步；1：同步成功；2：同步失败）',
  create_tm    timestamp(3)                            comment '创建时间',
  update_tm    timestamp(3)                            comment '更新时间',
  primary key (id) USING BTREE
)engine=innodb  comment ='摇号结果详情表' ROW_FORMAT = Dynamic;



drop table if exists lottery_apply_record;
create table lottery_apply_record(
  id           bigint(0)            not null           comment 'id',
  batch_id     bigint(0)            default 0          comment '摇号批次id',
  user_id      bigint(0)            default 0          comment '用户',
  plate_no     varchar(12)          default ''         comment '车牌号',
  apply_state  char(1)              default 0          comment '申请状态(0：取消申请；1：申请)',
  result       varchar(16)          default ''         comment '摇号结果(-1：未开号；0：未中；xx：对应停车场的区域编号)',
  create_tm    timestamp(3)                            comment '创建时间',
  update_tm    timestamp(3)                            comment '更新时间',
  primary key (id) USING BTREE
)engine=innodb  comment ='摇号申请记录表' ROW_FORMAT = Dynamic;





drop table if exists parking_space_transfer_record;
create table parking_space_transfer_record(
  id           bigint(0)            not null           comment 'id',
  parking_lot_id  bigint(0)         default 0          comment '转赠停车场id',
  user_id         bigint(0)         default 0          comment '申请人userId',
  accept_user_id  bigint(0)         default 0          comment '赠予人userId',
  accept_user_plate_no   varchar(12)    default ''     comment '赠予人车牌号',
  valid_start_date     date                            comment '转赠有效开始日期',
  valid_end_date       date                            comment '转赠有效截止日期',
  create_tm    timestamp(3)                            comment '创建时间',
  update_tm    timestamp(3)                            comment '更新时间',
  primary key (id) USING BTREE
)engine=innodb  comment ='车位转赠记录表' ROW_FORMAT = Dynamic;