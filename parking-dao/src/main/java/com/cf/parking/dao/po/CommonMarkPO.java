package com.cf.parking.dao.po;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;
import lombok.experimental.Accessors;

import java.util.Date;

@Data
@Accessors(chain = true)
@TableName("common_mark")
public class CommonMarkPO {
    @TableId(value = "common_mark_id", type = IdType.INPUT)
    private Long commonMarkId;

    private Integer sourceType;

    private String sourceCode;

    private Integer state;

    private Date createTm;

    private Date updateTm;

}