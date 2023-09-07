package com.cf.parking.facade.dto;

import com.fasterxml.jackson.annotation.JsonFormat;
import lombok.Data;
import lombok.experimental.Accessors;

import java.util.Date;

/**
 * 车辆审核
 * @author
 * @date 2023/9/7
 */
@Data
@Accessors(chain = true)
public class UserVerifyDTO {
    /** id */
    private Long id;

    /** 申请人 */
    private Long userName;

    /** 状态(0:默认，1:待审核，2:审核失败,3:审核成功) */
    private String state;

    /** 申请日期（起） */
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date startDate;

    /** 申请日期（止） */
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date updateTm;
}
