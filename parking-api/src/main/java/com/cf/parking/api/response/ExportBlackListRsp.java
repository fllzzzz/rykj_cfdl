package com.cf.parking.api.response;

import cn.afterturn.easypoi.excel.annotation.Excel;
import lombok.Data;
import lombok.experimental.Accessors;

/**
 * @author lpy
 * @date 2023-03-27 09:43:43
 * @description 黑名单记录表
 */
@Data
@Accessors(chain = true)
public class ExportBlackListRsp {
    private Long blackListId;

    @Excel(name = "加入时间", orderNum = "5")
    private String createTm;

    @Excel(name = " 是否删除", orderNum = "6")
    private String isDelete;

    @Excel(name = "工号", orderNum = "1")
    private String jobNumber;

    @Excel(name = "姓名", orderNum = "2")
    private String name;

    @Excel(name = "车牌号", orderNum = "3")
    private String plateNo;

    @Excel(name = "加入原因", orderNum = "4")
    private String joinReason;

}
